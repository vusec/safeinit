//===- SafeInit.cpp -------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a zero-initialization pass for alloca()ed memory.
// Note that this is done at LLVM IR level, not on the MC level.
//
//===----------------------------------------------------------------------===//

/*
 * This is the simplified release version of the pass, and has also
 * been modified to be not depend on anything outside the tree. Please be
 * careful with it and be sure to build LLVM in debug mode with asserts
 * when testing.
 */

#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Instrumentation.h"
using namespace llvm;

#define DEBUG_TYPE "safeinit"

// Initializing variables only when they're in-scope may not be a good idea;
// in particular, this means loop variables get repeatedly re-initialized.
// (Note that StackColoring will invalidate lifetimes if it sees memory stores
//  before the lifetime start, so we don't *have* to delete the intrinics right
//  now, although it might be a good idea anyway.)
static cl::opt<bool> IgnoreLifetimes ("STACKZEROINIT_IGNORELIFETIMES", cl::desc("Ignore lifetimes for allocas"), cl::init(false));

// this is for use in combination with framezeroinit only, don't use it
static cl::opt<bool> DynamicOnly ("STACKZEROINIT_DYNONLY", cl::desc("Only init dynamic allocas"), cl::init(false));

// Materialize memsets without lifetimes as late as possible (either
// just before their first uses, or dominating those uses).
static cl::opt<bool> MaterializeLate ("STACKZEROINIT_MATERIALIZELATE", cl::desc("Materialize alloca memsets as late as possible"), cl::init(true));

// Note that this is NOT SUPPORTED in the release version. To use it you
// may have to change other optimization passes, as well as the allocator,
// and we use a static LLVM-compile-time value in the release.
static cl::opt<bool> PoisonInit ("STACKZEROINIT_POISONINIT", cl::desc("Initialize allocas with a non-zero value"), cl::init(false));

STATISTIC(AllocaCounter, "Counts number of alloca calls with zero-initialization added");

namespace {
  struct SafeInit : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    SafeInit() : FunctionPass(ID) {}

    const TargetLibraryInfo *TLI;
    DominatorTree *DT;
    BasicBlock *Entry;
    unsigned memsetMDKind;
    unsigned nozeroinitMDKind;

    const char *getPassName() const { return "Stack Zero-Initialization"; }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.addRequired<DominatorTreeWrapperPass>();
      AU.addRequired<TargetLibraryInfoWrapperPass>();
    }

    bool runOnFunction(Function &F) override;

    bool isSafeStringArray(AllocaInst *AI, bool &sawNonTrivialUse);

    Instruction *findMatInsertPt(Instruction *Inst, unsigned Idx) const;
    BasicBlock *findCommonDominator(SetVector<BasicBlock *, SmallVector<BasicBlock *, 8> > &BBs) const;
    Instruction *findDominatingInsertionPoint(Instruction *I) const;

    bool addZeroInitForLifetimes(Module &M, Value *V, Instruction *I, Value *typesize, unsigned alignment);
    void addZeroInit(Module &M, Value *V, Instruction *I, Value *typesize, unsigned alignment);
  };

  struct HoistLifetimes : public LoopPass {
    static char ID; // Pass identification, replacement for typeid
    HoistLifetimes() : LoopPass(ID) {}

    DominatorTree *DT;

    const char *getPassName() const { return "Loop lifetime hoisting"; }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.addRequired<DominatorTreeWrapperPass>();
    }

    bool runOnLoop(Loop *L, LPPassManager &LPM) override;
  };
}

INITIALIZE_PASS(SafeInit, "safeinit",
    "SafeInit: initiailizes all the things.",
    false, false)

FunctionPass *llvm::createSafeInitPass() {
  return new SafeInit();
}

/* this is an example of how you could remove inits from SafeInit
   on the insertion side rather than the optimization side,
   however this is NOT NECESSARY for any of our results,
   and "safe" should be taken with a pinch of salt */
bool SafeInit::isSafeStringArray(AllocaInst *AI, bool &sawNonTrivialUse) {
  SetVector<Instruction *, SmallVector<Instruction *, 16> > Worklist;
  Worklist.insert(AI);

  // if we're poisoning with a non-zero value, string arrays are never safe
  if (PoisonInit)
    return false;

  for (unsigned int n = 0; n < Worklist.size(); ++n) {
    Instruction *WI = Worklist[n];
    for (Value::use_iterator U = WI->use_begin(), E = WI->use_end(); U != E; ++U) {
      Instruction *UI = dyn_cast<Instruction>(U->getUser());

      if (dyn_cast<CastInst>(UI) || dyn_cast<GetElementPtrInst>(UI)) {
        Worklist.insert(UI);
        continue;
      }

      if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(UI)) {
        switch (II->getIntrinsicID()) {
         case Intrinsic::lifetime_start:
         case Intrinsic::lifetime_end:
         case Intrinsic::dbg_declare:
         case Intrinsic::dbg_value:
           continue;
         default:
           // TODO: memset is also ok?
           return false;
        }
      }

      sawNonTrivialUse = true;

      if (auto CS = CallSite(UI)) {
        // Check if it's a string function.
        if (Function *F = CS.getCalledFunction()) {
          if (TLI->has(LibFunc::strcpy) &&
              F->getName() == TLI->getName(LibFunc::strcpy)) {
            if (U->getOperandNo() == 0) Worklist.insert(UI);
            continue;
          }
          if (TLI->has(LibFunc::strncpy) &&
              F->getName() == TLI->getName(LibFunc::strncpy)) {
            if (U->getOperandNo() == 0) Worklist.insert(UI);
            continue;
          }
          if (TLI->has(LibFunc::strcat) &&
              F->getName() == TLI->getName(LibFunc::strcat)) {
            if (U->getOperandNo() == 0) Worklist.insert(UI);
            continue;
          }
          if (TLI->has(LibFunc::strncat) &&
              F->getName() == TLI->getName(LibFunc::strncat)) {
            if (U->getOperandNo() == 0) Worklist.insert(UI);
            continue;
          }
          if (TLI->has(LibFunc::strcmp) &&
              F->getName() == TLI->getName(LibFunc::strcmp)) {
            continue;
          }
          if (TLI->has(LibFunc::strncmp) &&
              F->getName() == TLI->getName(LibFunc::strncmp)) {
            continue;
          }
          if (TLI->has(LibFunc::strlen) &&
              F->getName() == TLI->getName(LibFunc::strlen)) {
            continue;
          }
          // there's no dereference, so only relevant uses of sprintf/snprintf are as string
          if (TLI->has(LibFunc::sprintf) &&
              F->getName() == TLI->getName(LibFunc::sprintf)) {
            continue;
          }
          if (TLI->has(LibFunc::snprintf) &&
              F->getName() == TLI->getName(LibFunc::snprintf)) {
            continue;
          }
          if (TLI->has(LibFunc::printf) &&
              F->getName() == TLI->getName(LibFunc::printf)) {
            continue;
          }
          if (TLI->has(LibFunc::vprintf) &&
              F->getName() == TLI->getName(LibFunc::vprintf)) {
            continue;
          }
          if (TLI->has(LibFunc::vsprintf) &&
              F->getName() == TLI->getName(LibFunc::vsprintf)) {
            continue;
          }
          if (TLI->has(LibFunc::vsnprintf) &&
              F->getName() == TLI->getName(LibFunc::vsnprintf)) {
            continue;
          }
          if (TLI->has(LibFunc::fprintf) &&
              F->getName() == TLI->getName(LibFunc::fprintf)) {
            if (U->getOperandNo() != 0) // not the FILE*
              continue;
          }
          if (TLI->has(LibFunc::vfprintf) &&
              F->getName() == TLI->getName(LibFunc::vfprintf)) {
            if (U->getOperandNo() != 0) // not the FILE*
              continue;
          }
        }
      }

      // anything else is not ok
      // TODO: stores are also ok
      return false;
    }
  }

  if (sawNonTrivialUse)
    errs() << *AI << " is a safe string allocation\n";
  return true;
}

// this is derived from llvm's ConstantHoisting pass
Instruction *SafeInit::findMatInsertPt(Instruction *Inst, unsigned Idx) const {
  // The simple and common case. This also includes constant expressions.
  if (!isa<PHINode>(Inst) && !Inst->isEHPad())
    return Inst;

  // We can't insert directly before a phi node or an eh pad. Insert before
  // the terminator of the incoming or dominating block.
  assert(Entry != Inst->getParent() && "PHI or landing pad in entry block!");
  if (Idx != ~0U && isa<PHINode>(Inst))
    return cast<PHINode>(Inst)->getIncomingBlock(Idx)->getTerminator();

  BasicBlock *IDom = DT->getNode(Inst->getParent())->getIDom()->getBlock();
  return IDom->getTerminator();
}

// this is derived from llvm's ConstantHoisting pass
BasicBlock *SafeInit::findCommonDominator(SetVector<BasicBlock *, SmallVector<BasicBlock *, 8> > &BBs) const {
  while (BBs.size() >= 2) {
    BasicBlock *BB, *BB1, *BB2;
    BB1 = *BBs.begin();
    BB2 = *std::next(BBs.begin());
    BB = DT->findNearestCommonDominator(BB1, BB2);
    if (BB == Entry)
      return Entry;
    BBs.remove(BB1);
    BBs.remove(BB2);
    BBs.insert(BB);
  }
  assert((BBs.size() == 1) && "Expected only one element.");
  return *BBs.begin();
}

// this is from Analysis/CFG.cpp but we don't want the limit imposed there :-(
// TODO: put the LI stuff back?
static bool isPotentiallyReachableHacked(
    BasicBlock *From, BasicBlock *To,
    const DominatorTree *DT) {
  // When the stop block is unreachable, it's dominated from everywhere,
  // regardless of whether there's a path between the two blocks.
  if (DT && !DT->isReachableFromEntry(To))
    DT = nullptr;

  SmallVector<BasicBlock*, 32> Worklist;
  Worklist.push_back(const_cast<BasicBlock*>(From));
  // Limit the number of blocks we visit. The goal is to avoid run-away compile
  // times on large CFGs without hampering sensible code. Arbitrarily chosen.
  SmallPtrSet<const BasicBlock*, 32> Visited;
  do {
    BasicBlock *BB = Worklist.pop_back_val();
    if (!Visited.insert(BB).second)
      continue;
    if (BB == To)
      return true;
    if (DT && DT->dominates(BB, To))
      return true;

    Worklist.append(succ_begin(BB), succ_end(BB));
  } while (!Worklist.empty());

  // We have exhausted all possible paths and are certain that 'To' can not be
  // reached from 'From'.
  return false;
}

// returns the instruction which we should insert *before*
// (probably either a use, or a terminator)
Instruction *SafeInit::findDominatingInsertionPoint(Instruction *I) const {
  // Collect all basic blocks.
  SetVector<BasicBlock *, SmallVector<BasicBlock *, 8> > BBs;
  SmallPtrSet<Instruction *, 16> MatInsertPts;
  SmallPtrSet<Instruction *, 16> LifetimeStarts;

  SetVector<Instruction *, SmallVector<Instruction *, 16> > Worklist;
  Worklist.insert(I);

  for (unsigned int n = 0; n < Worklist.size(); ++n) {
    Instruction *WI = Worklist[n];
    for (Value::use_iterator U = WI->use_begin(), E = WI->use_end(); U != E; ++U) {
      Instruction *UI = dyn_cast<Instruction>(U->getUser());
      assert(UI && "uses must be instructions");
      assert(UI != I && "nope nope nope nope super nope");

      // We only follow casts/GEPs, and assume everything else is a 'real' use.
      // TODO: Be smarter.
      if (dyn_cast<CastInst>(UI) || dyn_cast<GetElementPtrInst>(UI)) {
        // Just follow.
        Worklist.insert(UI);
        continue;
      }

      // These intrinsics don't matter, though.
      if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(UI)) {
        switch (II->getIntrinsicID()) {
         case Intrinsic::lifetime_start:
           // We should insert *after* these (only happens in the first block right now).
           // TODO: (This only happens when other code is broken, right..?)
           LifetimeStarts.insert(II);
           continue;
         case Intrinsic::lifetime_end:
         case Intrinsic::dbg_declare:
         case Intrinsic::dbg_value:
           continue;
         default:
           break;
        }
      }

      Instruction *matInsertPt = findMatInsertPt(UI, U->getOperandNo());
      MatInsertPts.insert(matInsertPt);
      BBs.insert(matInsertPt->getParent());
    }
  }

  if (BBs.empty()) {
    // TODO: we should just give up on totally unused stuff? (think of poisoninit)
    DEBUG(dbgs() << "WARNING: stackzeroinit found no dom point (unused?) for " << *I << "\n");
    return NULL;
  }

  BasicBlock *dominatingBlock = Entry;

  // TODO: really we should be doing this properly (or at least cache the list)
  // for now: if there's an edge from a BB which reaches itself,
  // we just add all predecessors.
  for (unsigned int n = 0; n < BBs.size(); ++n) { // NOT an iterator
    BasicBlock *BB = BBs[n];
    // If this is the block in which the alloca is defined, we know that uses can only flow FROM here.
    if (BB == I->getParent())
      continue;
    for (auto SI = succ_begin(BB); SI != succ_end(BB); ++SI) {
      // if we can reach BB from this sucessor..
      if (*SI && isPotentiallyReachableHacked(*SI, BB, DT)) {
        // .. then add all predecessors of BB
        for (auto PI = pred_begin(BB); PI != pred_end(BB); ++PI)
          if (*PI)
            BBs.insert(*PI);
        break;
      }
    }
    if (BBs.count(Entry))
      break;
  }

  if (!BBs.count(dominatingBlock))
    dominatingBlock = findCommonDominator(BBs);

  Instruction *insertPoint = dominatingBlock->getFirstNonPHIOrDbg();
  // If the definition is in the dominating block, then the earliest point we can insert
  // is directly after the definition.
  // (If there's a lifetime, this also applies; should only happen in first block..)
  for (BasicBlock::iterator II = dominatingBlock->begin(), E = dominatingBlock->end(); II != E; ++II) {
    for (auto LS : LifetimeStarts) {
      if (&*II == LS)
        insertPoint = II->getNextNode();
    }
    if (&*II == I)
      insertPoint = II->getNextNode();
  }

  // Search until we find an instruction we can't insert (directly) before.
  while (insertPoint != dominatingBlock->getTerminator()) {
    if (MatInsertPts.count(insertPoint))
      break;
    insertPoint = insertPoint->getNextNode();
  }

  // once upon a time for dynamic allocas (see e.g. gcc's c-parse.c), the above generated bad inserts. should be fine now.
  // there's some more sanity checks here if you're paranoid.
  assert(DT->dominates(I, insertPoint) && "definition must dominate insertion point");
//  if (!DT->dominates(I, insertPoint))
//    insertPoint = I->getNextNode();
//  for (Value::use_iterator U = I->use_begin(), E = I->use_end(); U != E; ++U)
//    if (!DT->dominates(insertPoint, *U)) { I->getParent()->dump(); I->dump(); dyn_cast<Instruction>(*U)->dump(); assert(false); }
//    assert(((insertPoint == *U) || DT->dominates(insertPoint, *U)) && "insertion point must dominate (or be) user");

  DEBUG(dbgs() << "inserting at " << *insertPoint << " for " << *I << "\n");
  return insertPoint;
}

bool SafeInit::runOnFunction(Function &F) {
  bool MadeChanges = false;

  Module *M = F.getParent();
  const DataLayout &DL = M->getDataLayout();
  LLVMContext &C = M->getContext();

  memsetMDKind = C.getMDKindID("stackzeroinit");
  nozeroinitMDKind = C.getMDKindID("no_zeroinit");

  Entry = &F.getEntryBlock();

  DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  TLI = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();

  // Zero-initialize the return value of all alloca calls.
  for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I) {
      if (I->getMetadata(nozeroinitMDKind))
        continue;

      if (AllocaInst *AI = dyn_cast<AllocaInst>(I)) {
        AllocaCounter++;

        if (DynamicOnly && AI->isStaticAlloca()) continue;

        // Work out how many bytes are needed to store the type.
        // someone could investigate getTypeStoreSize, which returns the used bytes, vs getTypeAllocSize, which returns the number of allocated bytes
        // (for now we're conservative and use the latter, which also makes arrays easier)
        // NOTE: ScalarReplAggregates depends on the memset size matching getTypeAllocSize
        // (it uses getAllocatedType() rather than this..)

	Value *newsizeV;
        bool sawNonTrivialUse = false;
	// we (ab)use MaterializeLate as a flag for enabling this optimization too, for now
        if (MaterializeLate && isSafeStringArray(AI, sawNonTrivialUse)) {
          if (!sawNonTrivialUse)
            continue;
          // initialize only the first byte
          newsizeV = ConstantInt::get(Type::getInt64Ty(C), 1);
        } else {
        uint64_t typesize = DL.getTypeAllocSize(AI->getType()->getTypeAtIndex(0U));
        Value *typesizeV = ConstantInt::get(Type::getInt64Ty(C), typesize);
        Value *arraysizeV = AI->getArraySize();

        // Multiply the two sizes (CreateMul will optimise the constant case).
        IRBuilder<> irb(AI);
        Value *extArraysizeV = irb.CreateZExt(arraysizeV, typesizeV->getType());
        newsizeV = irb.CreateMul(typesizeV, extArraysizeV);
        }

        if (IgnoreLifetimes || !addZeroInitForLifetimes(*M, &*I, &*I, newsizeV, AI->getAlignment())) {
          if (MaterializeLate) {
            Instruction *IP = findDominatingInsertionPoint(&*I);
            if (IP)
              addZeroInit(*M, &*I, IP, newsizeV, AI->getAlignment());
          } else
            addZeroInit(*M, &*I, I->getNextNode(), newsizeV, AI->getAlignment());
        }
      }
    }
  }

  return MadeChanges;
}

// Zero-initialize based on the lifetime intrinsics, following bitcasts.
// Note that we don't pay attention to the size provided.
// Returns true if we found lifetimes, or false if we didn't
// (and so the caller potentially has to zero-init elsewhere).
bool SafeInit::addZeroInitForLifetimes(Module &M, Value *V, Instruction *I, Value *typesize, unsigned alignment) {
  bool FoundLifetimes = false;

  llvm::SmallSet<Value *, 8> Seen;
  for (Use &Us : I->uses()) {
    auto *U = dyn_cast<Instruction>(Us.getUser());
    if (!U)
      continue;
    if (Seen.count(U))
      continue;
    Seen.insert(U);

    // shouldn't need to follow more than just bitcasts here
    if (/*BitCastInst *BCI =*/ dyn_cast<BitCastInst>(U)) {
       FoundLifetimes |= addZeroInitForLifetimes(M, V, U, typesize, alignment);
    } else if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(U)) {
      if (II->getIntrinsicID() == Intrinsic::lifetime_start) {
        // note that we ignore lifetimes in the function entry
        if (MaterializeLate && U->getParent() == Entry)
          continue;
        FoundLifetimes = true;
        addZeroInit(M, V, U->getNextNode(), typesize, alignment);
      }
    }
  }

  return FoundLifetimes;
}

// insert zero-init instruction for V, immediately before I
void SafeInit::addZeroInit(Module &M, Value *V, Instruction *I, Value *typesize, unsigned alignment) {
  LLVMContext &C = M.getContext();
  IntegerType *Int1Ty = Type::getInt1Ty(C);
  IntegerType *Int8Ty = Type::getInt8Ty(C);
  Type *Int8PtrTy = Type::getInt8PtrTy(C);
  IntegerType *Int32Ty = Type::getInt32Ty(C);

  // Create a call to memset, with the same size as the value.
  std::vector<Type *> MemSetTys = { Int8PtrTy, typesize->getType() };
  Function *MemSetIntrinsic = Intrinsic::getDeclaration(&M, Intrinsic::memset, MemSetTys);

  Instruction *SI;

  // The memset intrinsic will get misoptimised if we pass anything other than an i8*.
  if (V->getType() != Int8PtrTy) {
    SI = new BitCastInst(V, Int8PtrTy);
    SI->insertBefore(I);
    V = SI;
  }

  // args: pointer (dst), value (zero), size, int32 align (0, for now at least), int1 volatile (0)
  Value *initValue = ConstantInt::get(Int8Ty, 0);
  if (PoisonInit)
    initValue = ConstantInt::get(Int8Ty, 0xcc); /* simplified for release */
  std::vector<Value*> args = { V, initValue, typesize, ConstantInt::get(Int32Ty, alignment), ConstantInt::get(Int1Ty, 0)};
  SI = CallInst::Create(MemSetIntrinsic, args);

  SI->setMetadata(memsetMDKind, MDNode::get(C, {}));

  // Insert the memset call just before the instruction.
  SI->insertBefore(I);
}

char SafeInit::ID = 0;

