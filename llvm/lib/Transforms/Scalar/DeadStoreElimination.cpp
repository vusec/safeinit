//===- DeadStoreElimination.cpp - Fast Dead Store Elimination -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements dead store elimination that considers redundant stores
// within a basic-block as well as across basic blocks in a reverse CFG order.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Local.h"
using namespace llvm;

#define DEBUG_TYPE "dse"

STATISTIC(NumRedundantStores, "Number of redundant stores deleted");
STATISTIC(NumFastStores, "Number of stores deleted");
STATISTIC(NumFastOther , "Number of other instrs removed");
STATISTIC(NumNonLocalStores, "Number of non-local stores deleted");

static cl::opt<bool> EnableNonLocalDSE("enable-nonlocal-dse", cl::init(true));
static cl::opt<bool> EnableSmallNonLocalDSE("enable-small-nonlocal-dse", cl::init(false));
extern cl::opt<bool> MallocReturnsZero;

/// MaxNonLocalAttempts is an arbitrary threshold that provides
/// an early opportunitiy for bail out to control compile time.
static const unsigned MaxNonLocalAttempts = 100;

namespace {
  struct DSE : public FunctionPass {
    AliasAnalysis *AA;
    MemoryDependenceResults *MD;
    DominatorTree *DT;
    const TargetLibraryInfo *TLI;

    static char ID; // Pass identification, replacement for typeid
    DSE() : FunctionPass(ID), AA(nullptr), MD(nullptr), DT(nullptr) {
      initializeDSEPass(*PassRegistry::getPassRegistry());
    }

    bool runOnFunction(Function &F) override {
      if (skipFunction(F))
        return false;

      AA = &getAnalysis<AAResultsWrapperPass>().getAAResults();
      MD = &getAnalysis<MemoryDependenceWrapperPass>().getMemDep();
      DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
      TLI = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();

      bool Changed = false;
      for (BasicBlock &I : F)
        // Only check non-dead blocks.  Dead blocks may have strange pointer
        // cycles that will confuse alias analysis.
        if (DT->isReachableFromEntry(&I))
          Changed |= runOnBasicBlock(I);

      AA = nullptr; MD = nullptr; DT = nullptr;
      return Changed;
    }

    bool runOnBasicBlock(BasicBlock &BB);
    bool MemoryIsNotModifiedBetween(Instruction *FirstI, Instruction *SecondI);
    bool HandleFree(CallInst *F);
    bool handleNonLocalDependency(Instruction *Inst);
    bool handleEndBlock(BasicBlock &BB);
    void RemoveAccessedObjects(const MemoryLocation &LoadedLoc,
                               SmallSetVector<Value *, 16> &DeadStackObjects,
                               const DataLayout &DL);

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesCFG();
      AU.addRequired<DominatorTreeWrapperPass>();
      AU.addRequired<AAResultsWrapperPass>();
      AU.addRequired<MemoryDependenceWrapperPass>();
      AU.addRequired<TargetLibraryInfoWrapperPass>();
      AU.addPreserved<DominatorTreeWrapperPass>();
      AU.addPreserved<GlobalsAAWrapperPass>();
      AU.addPreserved<MemoryDependenceWrapperPass>();
    }
  };
}

char DSE::ID = 0;
INITIALIZE_PASS_BEGIN(DSE, "dse", "Dead Store Elimination", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(GlobalsAAWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MemoryDependenceWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(DSE, "dse", "Dead Store Elimination", false, false)

FunctionPass *llvm::createDeadStoreEliminationPass() { return new DSE(); }

//===----------------------------------------------------------------------===//
// Helper functions
//===----------------------------------------------------------------------===//

/// DeleteDeadInstruction - Delete this instruction.  Before we do, go through
/// and zero out all the operands of this instruction.  If any of them become
/// dead, delete them and the computation tree that feeds them.
///
/// If ValueSet is non-null, remove any deleted instructions from it as well.
///
static void DeleteDeadInstruction(Instruction *I,
                               MemoryDependenceResults &MD,
                               const TargetLibraryInfo &TLI,
                               SmallSetVector<Value*, 16> *ValueSet = nullptr) {
  SmallVector<Instruction*, 32> NowDeadInsts;

  NowDeadInsts.push_back(I);
  --NumFastOther;

  // Before we touch this instruction, remove it from memdep!
  do {
    Instruction *DeadInst = NowDeadInsts.pop_back_val();
    ++NumFastOther;

    // This instruction is dead, zap it, in stages.  Start by removing it from
    // MemDep, which needs to know the operands and needs it to be in the
    // function.
    MD.removeInstruction(DeadInst);

    for (unsigned op = 0, e = DeadInst->getNumOperands(); op != e; ++op) {
      Value *Op = DeadInst->getOperand(op);
      DeadInst->setOperand(op, nullptr);

      // If this operand just became dead, add it to the NowDeadInsts list.
      if (!Op->use_empty()) continue;

      if (Instruction *OpI = dyn_cast<Instruction>(Op))
        if (isInstructionTriviallyDead(OpI, &TLI))
          NowDeadInsts.push_back(OpI);
    }

    DeadInst->eraseFromParent();

    if (ValueSet) ValueSet->remove(DeadInst);
  } while (!NowDeadInsts.empty());
}


/// hasMemoryWrite - Does this instruction write some memory?  This only returns
/// true for things that we can analyze with other helpers below.
static bool hasMemoryWrite(Instruction *I, const TargetLibraryInfo &TLI) {
  if (isa<StoreInst>(I))
    return true;
  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
    default:
      return false;
    case Intrinsic::memset:
    case Intrinsic::memmove:
    case Intrinsic::memcpy:
    case Intrinsic::init_trampoline:
    case Intrinsic::lifetime_end:
    case Intrinsic::initialized:
      return true;
    }
  }
  if (auto CS = CallSite(I)) {
    if (Function *F = CS.getCalledFunction()) {
      if (TLI.has(LibFunc::strcpy) &&
          F->getName() == TLI.getName(LibFunc::strcpy)) {
        return true;
      }
      if (TLI.has(LibFunc::strncpy) &&
          F->getName() == TLI.getName(LibFunc::strncpy)) {
        return true;
      }
      if (TLI.has(LibFunc::strcat) &&
          F->getName() == TLI.getName(LibFunc::strcat)) {
        return true;
      }
      if (TLI.has(LibFunc::strncat) &&
          F->getName() == TLI.getName(LibFunc::strncat)) {
        return true;
      }
    }
  }
  return false;
}

/// getLocForWrite - Return a Location stored to by the specified instruction.
/// If isRemovable returns true, this function and getLocForRead completely
/// describe the memory operations for this instruction.
static MemoryLocation getLocForWrite(Instruction *Inst, AliasAnalysis &AA) {
  if (StoreInst *SI = dyn_cast<StoreInst>(Inst))
    return MemoryLocation::get(SI);

  if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(Inst)) {
    // memcpy/memmove/memset.
    MemoryLocation Loc = MemoryLocation::getForDest(MI);
    return Loc;
  }

  IntrinsicInst *II = dyn_cast<IntrinsicInst>(Inst);
  if (!II)
    return MemoryLocation();

  switch (II->getIntrinsicID()) {
  default:
    return MemoryLocation(); // Unhandled intrinsic.
  case Intrinsic::init_trampoline:
    // FIXME: We don't know the size of the trampoline, so we can't really
    // handle it here.
    return MemoryLocation(II->getArgOperand(0));
  case Intrinsic::lifetime_end: {
    uint64_t Len = cast<ConstantInt>(II->getArgOperand(0))->getZExtValue();
    return MemoryLocation(II->getArgOperand(1), Len);
  }
  }
}

/// getLocForRead - Return the location read by the specified "hasMemoryWrite"
/// instruction if any.
static MemoryLocation getLocForRead(Instruction *Inst,
                                    const TargetLibraryInfo &TLI) {
  assert(hasMemoryWrite(Inst, TLI) && "Unknown instruction case");

  // The only instructions that both read and write are the mem transfer
  // instructions (memcpy/memmove).
  if (MemTransferInst *MTI = dyn_cast<MemTransferInst>(Inst))
    return MemoryLocation::getForSource(MTI);
  return MemoryLocation();
}


/// isRemovable - If the value of this instruction and the memory it writes to
/// is unused, may we delete this instruction?
static bool isRemovable(Instruction *I) {
  // Don't remove volatile/atomic stores.
  if (StoreInst *SI = dyn_cast<StoreInst>(I))
    return SI->isUnordered();

  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
    default: llvm_unreachable("doesn't pass 'hasMemoryWrite' predicate");
    case Intrinsic::lifetime_end:
      // Never remove dead lifetime_end's, e.g. because it is followed by a
      // free.
      return false;
    case Intrinsic::init_trampoline:
      // Always safe to remove init_trampoline.
      return true;

    case Intrinsic::memset:
    case Intrinsic::memmove:
    case Intrinsic::memcpy:
    case Intrinsic::initialized:
      // Don't remove volatile memory intrinsics.
      return !cast<MemIntrinsic>(II)->isVolatile();
    }
  }

  if (auto CS = CallSite(I))
    return CS.getInstruction()->use_empty();

  return false;
}


/// Returns true if the end of this instruction can be safely shortened in
/// length.
static bool isShortenableAtTheEnd(Instruction *I) {
  // Don't shorten stores for now
  if (isa<StoreInst>(I))
    return false;

  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
      default: return false;
      case Intrinsic::memset:
      case Intrinsic::memcpy:
        // Do shorten memory intrinsics.
        // FIXME: Add memmove if it's also safe to transform.
        return true;
    }
  }

  // Don't shorten libcalls calls for now.

  return false;
}

/// Returns true if the beginning of this instruction can be safely shortened
/// in length.
static bool isShortenableAtTheBeginning(Instruction *I) {
  // FIXME: Handle only memset for now. Supporting memcpy/memmove should be
  // easily done by offsetting the source address.
  IntrinsicInst *II = dyn_cast<IntrinsicInst>(I);
  return II && II->getIntrinsicID() == Intrinsic::memset;
}

/// getStoredPointerOperand - Return the pointer that is being written to.
static Value *getStoredPointerOperand(Instruction *I) {
  if (StoreInst *SI = dyn_cast<StoreInst>(I))
    return SI->getPointerOperand();
  if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(I))
    return MI->getDest();

  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
    default: llvm_unreachable("Unexpected intrinsic!");
    case Intrinsic::init_trampoline:
      return II->getArgOperand(0);
    }
  }

  CallSite CS(I);
  // All the supported functions so far happen to have dest as their first
  // argument.
  return CS.getArgument(0);
}

static uint64_t getPointerSize(const Value *V, const DataLayout &DL,
                               const TargetLibraryInfo &TLI) {
  uint64_t Size;
  if (getObjectSize(V, Size, DL, &TLI))
    return Size;
  return MemoryLocation::UnknownSize;
}

namespace {
enum OverwriteResult {
  OverwriteBegin,
  OverwriteComplete,
  OverwriteEnd,
  OverwriteUnknown
};
}

/// Return 'OverwriteComplete' if a store to the 'Later' location completely
/// overwrites a store to the 'Earlier' location, 'OverwriteEnd' if the end of
/// the 'Earlier' location is completely overwritten by 'Later',
/// 'OverwriteBegin' if the beginning of the 'Earlier' location is overwritten
/// by 'Later', or 'OverwriteUnknown' if nothing can be determined.
static OverwriteResult isOverwrite(const MemoryLocation &Later,
                                   const MemoryLocation &Earlier,
                                   const DataLayout &DL,
                                   const TargetLibraryInfo &TLI,
                                   int64_t &EarlierOff, int64_t &LaterOff) {
  const Value *P1 = Earlier.Ptr->stripPointerCasts();
  const Value *P2 = Later.Ptr->stripPointerCasts();

  // If the start pointers are the same, we just have to compare sizes to see if
  // the later store was larger than the earlier store.
  if (P1 == P2) {
    // If we don't know the sizes of either access, then we can't do a
    // comparison.
    if (Later.Size == MemoryLocation::UnknownSize ||
        Earlier.Size == MemoryLocation::UnknownSize)
      return OverwriteUnknown;

    // Make sure that the Later size is >= the Earlier size.
    if (Later.Size >= Earlier.Size)
      return OverwriteComplete;
  }

  // Otherwise, we have to have size information, and the later store has to be
  // larger than the earlier one.
  if (Later.Size == MemoryLocation::UnknownSize ||
      Earlier.Size == MemoryLocation::UnknownSize)
    return OverwriteUnknown;

  // Check to see if the later store is to the entire object (either a global,
  // an alloca, or a byval/inalloca argument).  If so, then it clearly
  // overwrites any other store to the same object.
  const Value *UO1 = GetUnderlyingObject(P1, DL),
              *UO2 = GetUnderlyingObject(P2, DL);

  // If we can't resolve the same pointers to the same object, then we can't
  // analyze them at all.
  if (UO1 != UO2)
    return OverwriteUnknown;

  // If the "Later" store is to a recognizable object, get its size.
  uint64_t ObjectSize = getPointerSize(UO2, DL, TLI);
  if (ObjectSize != MemoryLocation::UnknownSize)
    if (ObjectSize == Later.Size && ObjectSize >= Earlier.Size)
      return OverwriteComplete;

  // Okay, we have stores to two completely different pointers.  Try to
  // decompose the pointer into a "base + constant_offset" form.  If the base
  // pointers are equal, then we can reason about the two stores.
  EarlierOff = 0;
  LaterOff = 0;
  const Value *BP1 = GetPointerBaseWithConstantOffset(P1, EarlierOff, DL);
  const Value *BP2 = GetPointerBaseWithConstantOffset(P2, LaterOff, DL);

  // If the base pointers still differ, we have two completely different stores.
  if (BP1 != BP2)
    return OverwriteUnknown;

  // The later store completely overlaps the earlier store if:
  //
  // 1. Both start at the same offset and the later one's size is greater than
  //    or equal to the earlier one's, or
  //
  //      |--earlier--|
  //      |--   later   --|
  //
  // 2. The earlier store has an offset greater than the later offset, but which
  //    still lies completely within the later store.
  //
  //        |--earlier--|
  //    |-----  later  ------|
  //
  // We have to be careful here as *Off is signed while *.Size is unsigned.
  if (EarlierOff >= LaterOff &&
      Later.Size >= Earlier.Size &&
      uint64_t(EarlierOff - LaterOff) + Earlier.Size <= Later.Size)
    return OverwriteComplete;

  // Another interesting case is if the later store overwrites the end of the
  // earlier store.
  //
  //      |--earlier--|
  //                |--   later   --|
  //
  // In this case we may want to trim the size of earlier to avoid generating
  // writes to addresses which will definitely be overwritten later
  if (LaterOff > EarlierOff &&
      LaterOff < int64_t(EarlierOff + Earlier.Size) &&
      int64_t(LaterOff + Later.Size) >= int64_t(EarlierOff + Earlier.Size))
    return OverwriteEnd;

  // Finally, we also need to check if the later store overwrites the beginning
  // of the earlier store.
  //
  //                |--earlier--|
  //      |--   later   --|
  //
  // In this case we may want to move the destination address and trim the size
  // of earlier to avoid generating writes to addresses which will definitely
  // be overwritten later.
  if (LaterOff <= EarlierOff && int64_t(LaterOff + Later.Size) > EarlierOff) {
    assert (int64_t(LaterOff + Later.Size) < int64_t(EarlierOff + Earlier.Size)
            && "Expect to be handled as OverwriteComplete" );
    return OverwriteBegin;
  }
  // Otherwise, they don't completely overlap.
  return OverwriteUnknown;
}

/// isPossibleSelfRead - If 'Inst' might be a self read (i.e. a noop copy of a
/// memory region into an identical pointer) then it doesn't actually make its
/// input dead in the traditional sense.  Consider this case:
///
///   memcpy(A <- B)
///   memcpy(A <- A)
///
/// In this case, the second store to A does not make the first store to A dead.
/// The usual situation isn't an explicit A<-A store like this (which can be
/// trivially removed) but a case where two pointers may alias.
///
/// This function detects when it is unsafe to remove a dependent instruction
/// because the DSE inducing instruction may be a self-read.
static bool isPossibleSelfRead(Instruction *Inst,
                               const MemoryLocation &InstStoreLoc,
                               Instruction *DepWrite,
                               const TargetLibraryInfo &TLI,
                               AliasAnalysis &AA) {
  // Self reads can only happen for instructions that read memory.  Get the
  // location read.
  MemoryLocation InstReadLoc = getLocForRead(Inst, TLI);
  if (!InstReadLoc.Ptr) return false;  // Not a reading instruction.

  // If the read and written loc obviously don't alias, it isn't a read.
  if (AA.isNoAlias(InstReadLoc, InstStoreLoc)) return false;

  // Okay, 'Inst' may copy over itself.  However, we can still remove a the
  // DepWrite instruction if we can prove that it reads from the same location
  // as Inst.  This handles useful cases like:
  //   memcpy(A <- B)
  //   memcpy(A <- B)
  // Here we don't know if A/B may alias, but we do know that B/B are must
  // aliases, so removing the first memcpy is safe (assuming it writes <= #
  // bytes as the second one.
  MemoryLocation DepReadLoc = getLocForRead(DepWrite, TLI);

  if (DepReadLoc.Ptr && AA.isMustAlias(InstReadLoc.Ptr, DepReadLoc.Ptr))
    return false;

  // If DepWrite doesn't read memory or if we can't prove it is a must alias,
  // then it can't be considered dead.
  return true;
}


//===----------------------------------------------------------------------===//
// DSE Pass
//===----------------------------------------------------------------------===//

bool DSE::runOnBasicBlock(BasicBlock &BB) {
  const DataLayout &DL = BB.getModule()->getDataLayout();
  bool MadeChange = false;
  unsigned NumNonLocalAttempts = 0;

  // Do a top-down walk on the BB.
  for (BasicBlock::iterator BBI = BB.begin(), BBE = BB.end(); BBI != BBE; ) {
    Instruction *Inst = &*BBI++;

    // Handle 'free' calls specially.
    if (CallInst *F = isFreeCall(Inst, TLI)) {
      MadeChange |= HandleFree(F);
      continue;
    }

    // If we find something that writes memory, get its memory dependence.
    if (!hasMemoryWrite(Inst, *TLI))
      continue;

      auto RemoveDeadInstAndUpdateBBI = [&](Instruction *DeadInst) {
        // DeleteDeadInstruction can delete the current instruction.  Save BBI
        // in case we need it.
        WeakVH NextInst(&*BBI);

        DeleteDeadInstruction(DeadInst, *MD, *TLI);

        if (!NextInst) // Next instruction deleted.
          BBI = BB.begin();
        else if (BBI != BB.begin()) // Revisit this instruction if possible.
          --BBI;
        ++NumRedundantStores;
        MadeChange = true;
      };

    // If we're storing the same value back to a pointer that we just
    // loaded from, then the store can be removed.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      if (LoadInst *DepLoad = dyn_cast<LoadInst>(SI->getValueOperand())) {
        if (SI->getPointerOperand() == DepLoad->getPointerOperand() &&
            isRemovable(SI) &&
            MemoryIsNotModifiedBetween(DepLoad, SI)) {

          DEBUG(dbgs() << "DSE: Remove Store Of Load from same pointer:\n  "
                       << "LOAD: " << *DepLoad << "\n  STORE: " << *SI << '\n');

          RemoveDeadInstAndUpdateBBI(SI);
          continue;
        }
      }

      // Remove null stores into the calloc'ed objects
      Constant *StoredConstant = dyn_cast<Constant>(SI->getValueOperand());

      if (StoredConstant && StoredConstant->isNullValue() &&
          isRemovable(SI)) {
        Instruction *UnderlyingPointer = dyn_cast<Instruction>(
            GetUnderlyingObject(SI->getPointerOperand(), DL));

        bool isCallocLike = UnderlyingPointer && isCallocLikeFn(UnderlyingPointer, TLI);
        isCallocLike |= (UnderlyingPointer && MallocReturnsZero && isMallocLikeFn(UnderlyingPointer, TLI));
        if (UnderlyingPointer && isCallocLike &&
            MemoryIsNotModifiedBetween(UnderlyingPointer, SI)) {
          DEBUG(dbgs()
                << "DSE: Remove null store to the calloc'ed object:\n  DEAD: "
                << *Inst << "\n  OBJECT: " << *UnderlyingPointer << '\n');

          RemoveDeadInstAndUpdateBBI(SI);
          continue;
        }
      }
    }

    // remove null memset instructions into calloc'ed stuff
    if (MemSetInst *MSI = dyn_cast<MemSetInst>(Inst)) {
      Value *V = MSI->getValue();
      Value *Dest = MSI->getDest();
      Instruction *UnderlyingPointer = dyn_cast<Instruction>(GetUnderlyingObject(Dest, DL));

      Constant *StoredConstant = dyn_cast<Constant>(V);
      if (StoredConstant && StoredConstant->isNullValue() && isRemovable(MSI)) {
        bool isCallocLike = UnderlyingPointer && isCallocLikeFn(UnderlyingPointer, TLI);
        isCallocLike |= (UnderlyingPointer && MallocReturnsZero && isMallocLikeFn(UnderlyingPointer, TLI));
        if (UnderlyingPointer && isCallocLike &&
            MemoryIsNotModifiedBetween(UnderlyingPointer, MSI)) {
          DEBUG(dbgs()
                << "DSE: Remove null memset to the calloc'ed object:\n  DEAD: "
                << *Inst << "\n  OBJECT: " << *UnderlyingPointer << '\n');

          RemoveDeadInstAndUpdateBBI(MSI);
          continue;
        }
      }
    }

    MemDepResult InstDep = MD->getDependency(Inst);

    if (InstDep.isDef() || InstDep.isClobber()) {
      // Figure out what location is being stored to.
      MemoryLocation Loc = getLocForWrite(Inst, *AA);

      // If we didn't get a useful location, fail.
      if (!Loc.Ptr)
        continue;

      while (InstDep.isDef() || InstDep.isClobber()) {
        // Get the memory clobbered by the instruction we depend on.  MemDep
        // will skip any instructions that 'Loc' clearly doesn't interact with.
        // If we end up depending on a may- or must-aliased load, then we can't
        // optimize away the store and we bail out.  However, if we depend on on
        // something that overwrites the memory location we *can* potentially
        // optimize it.
        //
        // Find out what memory location the dependent instruction stores.
        Instruction *DepWrite = InstDep.getInst();
        MemoryLocation DepLoc = getLocForWrite(DepWrite, *AA);
        // If we didn't get a useful location, or if it isn't a size, bail out.
        if (!DepLoc.Ptr)
          break;

        // If we find a write that is a) removable (i.e., non-volatile), b) is
        // completely obliterated by the store to 'Loc', and c) which we know
        // that 'Inst' doesn't load from, then we can remove it.
        if (isRemovable(DepWrite) &&
            !isPossibleSelfRead(Inst, Loc, DepWrite, *TLI, *AA)) {
          int64_t InstWriteOffset, DepWriteOffset;
          OverwriteResult OR = isOverwrite(Loc, DepLoc, DL, *TLI,
                                           DepWriteOffset, InstWriteOffset);
          if (OR == OverwriteComplete) {
            DEBUG(dbgs() << "DSE: Remove Dead Store:\n  DEAD: " << *DepWrite
                         << "\n  KILLER: " << *Inst << '\n');

            // Delete the store and now-dead instructions that feed it.
            DeleteDeadInstruction(DepWrite, *MD, *TLI);
            ++NumFastStores;
            MadeChange = true;

            // DeleteDeadInstruction can delete the current instruction in loop
            // cases, reset BBI.
            BBI = Inst->getIterator();
            auto BBBegin = BB.begin();
            while (BBI != BBBegin && isa<DbgInfoIntrinsic>(*(--BBI))) { }
            break;
          } else if ((OR == OverwriteEnd && isShortenableAtTheEnd(DepWrite)) ||
                     ((OR == OverwriteBegin &&
                       isShortenableAtTheBeginning(DepWrite)))) {
            // TODO: base this on the target vector size so that if the earlier
            // store was too small to get vector writes anyway then its likely
            // a good idea to shorten it
            // Power of 2 vector writes are probably always a bad idea to optimize
            // as any store/memset/memcpy is likely using vector instructions so
            // shortening it to not vector size is likely to be slower
            MemIntrinsic *DepIntrinsic = cast<MemIntrinsic>(DepWrite);
            unsigned DepWriteAlign = DepIntrinsic->getAlignment();
            bool IsOverwriteEnd = (OR == OverwriteEnd);
            if (!IsOverwriteEnd)
              InstWriteOffset = int64_t(InstWriteOffset + Loc.Size);

            if ((llvm::isPowerOf2_64(InstWriteOffset) &&
                 DepWriteAlign <= InstWriteOffset) ||
                ((DepWriteAlign != 0) && InstWriteOffset % DepWriteAlign == 0)) {

              DEBUG(dbgs() << "DSE: Remove Dead Store:\n  OW "
                           << (IsOverwriteEnd ? "END" : "BEGIN") << ": "
                           << *DepWrite << "\n  KILLER (offset "
                           << InstWriteOffset << ", " << DepLoc.Size << ")"
                           << *Inst << '\n');

              int64_t NewLength =
                  IsOverwriteEnd
                      ? InstWriteOffset - DepWriteOffset
                      : DepLoc.Size - (InstWriteOffset - DepWriteOffset);

              Value *DepWriteLength = DepIntrinsic->getLength();
              Value *TrimmedLength =
                  ConstantInt::get(DepWriteLength->getType(), NewLength);
              DepIntrinsic->setLength(TrimmedLength);

              if (!IsOverwriteEnd) {
                int64_t OffsetMoved = (InstWriteOffset - DepWriteOffset);
                Value *Indices[1] = {
                    ConstantInt::get(DepWriteLength->getType(), OffsetMoved)};
                GetElementPtrInst *NewDestGEP = GetElementPtrInst::CreateInBounds(
                    DepIntrinsic->getRawDest(), Indices, "", DepWrite);
                DepIntrinsic->setDest(NewDestGEP);
              }

              MadeChange = true;
            }
          } else if (MemIntrinsic *NewIntrinsic = dyn_cast<MemIntrinsic>(Inst)) {
            // We want to shorten a memset, where there's a second non-constant store.
            // TODO: if both of the stores are doing the same thing, why don't we just kill the second store instead...
            //errs() << "normal DSE failed for: " << *DepWrite << " " << *Inst << "\n";
            // DepLoc is earlier, Loc is later
            const Value *P1 = DepLoc.Ptr->stripPointerCasts();
            const Value *P2 = Loc.Ptr->stripPointerCasts();
            MemSetInst *DepIntrinsic = dyn_cast<MemSetInst>(DepWrite);
            // note there is a self-read check above

            uint64_t ObjectSize = getPointerSize(P1, DL, *TLI);

            bool SizeIsGood = true;

            // can we clobber the original entirely?
            if (DepIntrinsic) {
              Value *OldV = DepIntrinsic->getLength();
              Value *NewV = NewIntrinsic->getLength();
              if ((P1 == P2) && (OldV == NewV)) {
                // this is a complete overwrite! the old memset can be removed

                DEBUG(dbgs() << "DSE: Remove Dead Dyn-Size Store:\n  DEAD: " << *DepWrite
                         << "\n  KILLER: " << *Inst << '\n');

                // dup of code from above
                DeleteDeadInstruction(DepWrite, *MD, *TLI);
                ++NumFastStores;
                MadeChange = true;
                BBI = Inst->getIterator();
                auto BBBegin = BB.begin();
                while (BBI != BBBegin && isa<DbgInfoIntrinsic>(*(--BBI))) { }
                break;
              }
            }

            if (ObjectSize < 64) SizeIsGood = false; // there's no point shortening tiny stores
            Value *sizeV;
            Instruction *sizeInst;
            if (NewIntrinsic && SizeIsGood) {
              sizeV = NewIntrinsic->getLength();
 
              // apply super conservative security requirements (aka ruin everything)

              // calculate approximate range (modified from SimplifyLibCalls, see comments there)
              unsigned BitWidth = sizeV->getType()->getIntegerBitWidth();
              APInt KnownZero(BitWidth, 0);
              APInt KnownOne(BitWidth, 0);
              computeKnownBits(sizeV, KnownZero, KnownOne, DL, 0, nullptr, DepIntrinsic,
                               nullptr);
              KnownZero.flipAllBits();

              if (KnownZero.isNonNegative() && KnownZero.ule(ObjectSize)) {
                // size can't exceed object size -> good
              } else {
                // TODO: we could try SCEV here..
                SizeIsGood = false;
              }
            }

            if (SizeIsGood &&
             (P1 == P2) && // pointers must be the same (not just the underlying ones)
             DepIntrinsic && NewIntrinsic && // we must be (potentially) overwriting a memset
             (ObjectSize != MemoryLocation::UnknownSize) && (ObjectSize == DepLoc.Size)) { // the overwritten memset must cover the entire object (so we don't have to check the offset, TODO: do so anyway)
              // If the size of the later write ('x') dominates both instructions,
              // then we can shorten the original write (changing the pointer to ptr+x and the size to size-x).
              // This is ONLY 'safe' if control is guaranteed to reach the second instruction
              // whenever it reaches the first. In that case: the later write would overflow anyway.
              // (This does turn e.g. a single-byte overwrite into a crash, which is unfortunate.)
              // Look into adding some checks here, but not needed atm due to check above, and anyway we could just assume some other protection does it..
              errs() << "-> non-constant DSE is considering: " << *DepWrite << " overwritten by " << *Inst << ", ptr " << *P1 << ", len " << sizeV << "\n";

              while (isa<SExtInst>(sizeV) || isa<ZExtInst>(sizeV)) {
                // strip size extensions
                sizeV = cast<CastInst>(sizeV)->getOperand(0);
              }
              sizeInst = dyn_cast<Instruction>(sizeV);
 
              // if the size is an instruction, it must dominate the first instruction
              // (otherwise it might be e.g. a function argument or a global)
              if (!sizeInst || DT->dominates(sizeInst, DepIntrinsic)) {
                // there might be a function call in between the two which could abort()
                // for safety's sake, we make sure there isn't
                BasicBlock::const_iterator DDI(DepWrite);
                for (; &*DDI && &*DDI != Inst; ++DDI) {
                  //errs() << "-> considering: " << *DDI << "\n";
                  if (const CallInst *CI = dyn_cast<CallInst>(&*DDI)) {
                    // FIXME: isn't there a better check for this?
                    if (!dyn_cast<IntrinsicInst>(CI))
                      break;
                  }
                  // note that the length isn't a pointer, so we don't need to check for AA..
                }
                if (&*DDI == Inst) {
                  errs() << "-> looks safe, shortening\n";

                  // (note: the parameters to memset are always bytes, so counting in bytes is good)
                  IRBuilder<> Builder(DepIntrinsic);

                  // rewrite the pointer to have an offset of *sizeV
                  // (do we ever need the pointer cast here? probably not..)
                  Value *ptrBase = Builder.CreatePointerCast(DepIntrinsic->getRawDest(), Builder.getInt8PtrTy());
                  Value *zextSizeV = Builder.CreateZExtOrTrunc(sizeV, Builder.getInt64Ty());
                  // we won't get a trunc because we didn't strip any above
                  Value *Indices[1] = { zextSizeV };
                  GetElementPtrInst *NewDestGEP = GetElementPtrInst::CreateInBounds(
                      ptrBase, Indices, "", DepWrite);
                  DepIntrinsic->setDest(NewDestGEP);

                  // rewrite the length to be ObjectSize - *sizeV
                  // we know the earlier write is full-length which helps
                  Value *newLength = Builder.CreateSub(Builder.CreateZExtOrTrunc(DepIntrinsic->getLength(), Builder.getInt64Ty()), zextSizeV);
                  DepIntrinsic->setLength(newLength);

                  MadeChange = true;
                }
              }
            }
          }
        }

        // If this is a may-aliased store that is clobbering the store value, we
        // can keep searching past it for another must-aliased pointer that
        // stores to the same location.  For example, in
        //   store -> P
        //   store -> Q
        //   store -> P
        // we can remove the first store to P even though we don't know if P and
        // Q alias.
        if (DepWrite == &BB.front())
          break;

        // Can't look past this instruction if it might read 'Loc'.
        if (AA->getModRefInfo(DepWrite, Loc) & MRI_Ref)
          break;

        InstDep = MD->getPointerDependencyFrom(Loc, false,
                                               DepWrite->getIterator(), &BB);
      }
    } else if (EnableNonLocalDSE && InstDep.isNonLocal()) { // DSE across BB
      if (++NumNonLocalAttempts < MaxNonLocalAttempts)
        MadeChange |= handleNonLocalDependency(Inst);
    }
  }

  // If this block ends in a return, unwind, or unreachable, all allocas are
  // dead at its end, which means stores to them are also dead.
  if (BB.getTerminator()->getNumSuccessors() == 0)
    MadeChange |= handleEndBlock(BB);

  return MadeChange;
}

/// A helper for handleNonLocalDependency() function to find all blocks
/// that lead to the input block BB and append them to the output PredBlocks.
/// PredBlocks will include not only predecessors of BB that unconditionally
/// lead to BB but also:
///   - single-block loops that lead to BB, and
///   - if-blocks for which one edge goes to BB and the other edge goes to
///     a block in the input SafeBlocks.
/// PredBlocks will not include blocks unreachable from the entry block, nor
/// blocks that form cycles with BB.
static void findSafePreds(SmallVectorImpl<BasicBlock *> &PredBlocks,
                          SmallSetVector<BasicBlock *, 8> &SafeBlocks,
                          BasicBlock *BB, DominatorTree *DT) {
  for (auto I = pred_begin(BB), E = pred_end(BB); I != E; ++I) {
    BasicBlock *Pred = *I;
    if (Pred == BB)
      continue;
    // The second check below prevents adding blocks that form a cycle with BB
    // in order to avoid potential problems due to MemoryDependenceAnalysis,
    // isOverwrite, etc. being not loop-aware.
    if (!DT->isReachableFromEntry(Pred) || DT->dominates(BB, Pred))
      continue;

    bool PredIsSafe = true;
    for (auto II = succ_begin(Pred), EE = succ_end(Pred); II != EE; ++II) {
      BasicBlock *Succ = *II;
      if (Succ == BB || Succ == Pred) // shortcut, BB should be in SafeBlocks
        continue;
      if (!SafeBlocks.count(Succ)) {
        PredIsSafe = false;
        break;
      }
    }
    if (PredIsSafe)
      PredBlocks.push_back(Pred);
  }
}

static bool underlyingObjectsDoNotAlias(Instruction *Inst, LoadInst *LI,
                                        const DataLayout &DL,
                                        AliasAnalysis &AA) {
  Value *AObj;
  if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) AObj = GetUnderlyingObject(SI->getPointerOperand(), DL);
  else AObj = GetUnderlyingObject(dyn_cast<MemSetInst>(Inst)->getDest(), DL);
  SmallVector<Value *, 4> Pointers;
  GetUnderlyingObjects(LI->getPointerOperand(), Pointers, DL);

  for (auto I = Pointers.begin(), E = Pointers.end(); I != E; ++I) {
    Value *BObj = *I;
    if (!AA.isNoAlias(AObj, DL.getTypeStoreSize(AObj->getType()), BObj,
                      DL.getTypeStoreSize(BObj->getType())))
      return false;
  }
  return true;
}

/// handleNonLocalDependency - Handle a non-local dependency on
/// the input instruction Inst located in BB in attempt to remove
/// redundant stores outside BB.
bool DSE::handleNonLocalDependency(Instruction *Inst) {
  auto *SI = dyn_cast<StoreInst>(Inst);
  auto *MSI = dyn_cast<MemSetInst>(Inst);
  if (!SI && !MSI)
    return false;
  // Get the location being stored to.
  // If we don't get a useful location, bail out.
  MemoryLocation Loc = getLocForWrite(Inst, *AA);
  if (!Loc.Ptr)
    return false;

  bool MadeChange = false;
  BasicBlock *BB = Inst->getParent();
  const DataLayout &DL = BB->getModule()->getDataLayout();

  // Worklist of predecessor blocks of BB
  SmallVector<BasicBlock *, 8> Blocks;
  // Keep track of all predecessor blocks that are safe to search through
  SmallSetVector<BasicBlock *, 8> SafeBlocks;
  SafeBlocks.insert(BB);
  findSafePreds(Blocks, SafeBlocks, BB, DT);

  while (!Blocks.empty()) {
    BasicBlock *PB = Blocks.pop_back_val();
    MemDepResult Dep =
        MD->getPointerDependencyFrom(Loc, false, PB->end(), PB, Inst);
    while (Dep.isDef() || Dep.isClobber()) {
      Instruction *Dependency = Dep.getInst();
      DEBUG(dbgs() << "DSE: considering cross-block dependency " << *Dependency << "\n");

      // Filter out false dependency from a load to SI looking through phis.
      if (auto *LI = dyn_cast<LoadInst>(Dependency)) {
        if (underlyingObjectsDoNotAlias(Inst, LI, DL, *AA)) {
          Dep = MD->getPointerDependencyFrom(Loc, false,
                                             Dependency->getIterator(), PB, Inst);
          continue;
        }
      }

      // If we don't get a useful location for the dependent instruction,
      // it doesn't write memory, it is not removable, or it might read Loc,
      // then bail out.
      MemoryLocation DepLoc = getLocForWrite(Dependency, *AA);
      if (!DepLoc.Ptr || !hasMemoryWrite(Dependency, *TLI) ||
          !isRemovable(Dependency) ||
          (AA->getModRefInfo(Dependency, Loc) & MRI_Ref))
        break;

      // Don't remove a store within single-block loops;
      // we need more analysis: e.g. looking for an interferring load
      // above the store within the loop, etc.
      bool SingleBlockLoop = false;
      for (auto I = succ_begin(PB), E = succ_end(PB); I != E; ++I) {
        BasicBlock *Succ = *I;
        if (Succ == PB) {
          SingleBlockLoop = true;
          break;
        }
      }
      if (SingleBlockLoop)
        break;

      int64_t InstWriteOffset, DepWriteOffset;
      OverwriteResult OR =
          isOverwrite(Loc, DepLoc, DL, *TLI, DepWriteOffset, InstWriteOffset);
      if (OR == OverwriteComplete) {
        DEBUG(dbgs() << "DSE: Remove Non-Local Dead Store:\n  DEAD: "
                     << *Dependency << "\n  KILLER: " << *Inst << '\n');

//      if (!EnableSmallNonLocalDSE && DepLoc.Size <= 64)
      if (!EnableSmallNonLocalDSE && DepLoc.Size <= 8)
        break; // XXX alyssa did this to avoid crazy perf regressions (in perlbench)

        // Delete redundant store and now-dead instructions that feed it.
        auto Next = std::next(Dependency->getIterator());
        DeleteDeadInstruction(Dependency, *MD, *TLI);
        ++NumNonLocalStores;
        MadeChange = true;
        Dep = MD->getPointerDependencyFrom(Loc, false, Next, PB, Inst);
        continue;
      }
      // XXX: this is attempting shortening of Dependency inst as in the local case
      else if ((OR == OverwriteEnd && isShortenableAtTheEnd(Dependency)) ||
                     ((OR == OverwriteBegin &&
                       isShortenableAtTheBeginning(Dependency)))) {

//      if (!EnableSmallNonLocalDSE && DepLoc.Size <= 64)
      if (!EnableSmallNonLocalDSE && DepLoc.Size <= 8)
        break; // XXX alyssa did this to avoid crazy perf regressions (in perlbench)

            // TODO: base this on the target vector size so that if the earlier
            // store was too small to get vector writes anyway then its likely
            // a good idea to shorten it
            // Power of 2 vector writes are probably always a bad idea to optimize
            // as any store/memset/memcpy is likely using vector instructions so
            // shortening it to not vector size is likely to be slower
            MemIntrinsic *DepIntrinsic = cast<MemIntrinsic>(Dependency);
            unsigned DepWriteAlign = DepIntrinsic->getAlignment();
            bool IsOverwriteEnd = (OR == OverwriteEnd);
            if (!IsOverwriteEnd)
              InstWriteOffset = int64_t(InstWriteOffset + Loc.Size);

            if ((llvm::isPowerOf2_64(InstWriteOffset) &&
                 DepWriteAlign <= InstWriteOffset) ||
                ((DepWriteAlign != 0) && InstWriteOffset % DepWriteAlign == 0)) {

              DEBUG(dbgs() << "DSE: Cross-Block Remove Dead Store:\n  OW "
                           << (IsOverwriteEnd ? "END" : "BEGIN") << ": "
                           << *Dependency << "\n  KILLER (offset "
                           << InstWriteOffset << ", " << DepLoc.Size << ")"
                           << *Inst << '\n');

              int64_t NewLength =
                  IsOverwriteEnd
                      ? InstWriteOffset - DepWriteOffset
                      : DepLoc.Size - (InstWriteOffset - DepWriteOffset);

              Value *DepWriteLength = DepIntrinsic->getLength();
              Value *TrimmedLength =
                  ConstantInt::get(DepWriteLength->getType(), NewLength);
              DepIntrinsic->setLength(TrimmedLength);

              if (!IsOverwriteEnd) {
                int64_t OffsetMoved = (InstWriteOffset - DepWriteOffset);
                Value *Indices[1] = {
                    ConstantInt::get(DepWriteLength->getType(), OffsetMoved)};
                GetElementPtrInst *NewDestGEP = GetElementPtrInst::CreateInBounds(
                    DepIntrinsic->getRawDest(), Indices, "", Dependency);
                DepIntrinsic->setDest(NewDestGEP);
              }

              MadeChange = true;
              auto Next = std::next(Dependency->getIterator());
              Dep = MD->getPointerDependencyFrom(Loc, false, Next, PB, Inst);
              continue;
            }
          } else if (MemIntrinsic *NewIntrinsic = dyn_cast<MemIntrinsic>(Inst)) {
            // We want to shorten a memset, where there's a second non-constant store.
            // TODO: if both of the stores are doing the same thing, why don't we just kill the second store instead...
            //errs() << "normal DSE failed for: " << *DepWrite << " " << *Inst << "\n";
            // DepLoc is earlier, Loc is later
            const Value *P1 = DepLoc.Ptr->stripPointerCasts();
            const Value *P2 = Loc.Ptr->stripPointerCasts();
            MemSetInst *DepIntrinsic = dyn_cast<MemSetInst>(Dependency);
            // note there is a self-read check above

            // can we clobber the original entirely?
            if (DepIntrinsic) {
              Value *OldV = DepIntrinsic->getLength();
              Value *NewV = NewIntrinsic->getLength();
              if ((P1 == P2) && (OldV == NewV)) {
                // this is a complete overwrite! the old memset can be removed

                DEBUG(dbgs() << "DSE: Remove Non-Local Dead Dyn-Size Store:\n  DEAD: " << *Dependency
                         << "\n  KILLER: " << *Inst << '\n');

                // dup of code from above
                auto Next = std::next(Dependency->getIterator());
                DeleteDeadInstruction(Dependency, *MD, *TLI);
                ++NumNonLocalStores;
                MadeChange = true;
                Dep = MD->getPointerDependencyFrom(Loc, false, Next, PB, Inst);
                continue;
              }
            }
         }
      break;
    }

    if (Dep.isNonLocal()) {
      SafeBlocks.insert(PB);
      findSafePreds(Blocks, SafeBlocks, PB, DT);
    }
  }

  return MadeChange;
}

/// Returns true if the memory which is accessed by the second instruction is not
/// modified between the first and the second instruction.
/// Precondition: Second instruction must be dominated by the first
/// instruction.
bool DSE::MemoryIsNotModifiedBetween(Instruction *FirstI,
                                     Instruction *SecondI) {
  SmallVector<BasicBlock *, 16> WorkList;
  SmallPtrSet<BasicBlock *, 8> Visited;
  BasicBlock::iterator FirstBBI(FirstI);
  ++FirstBBI;
  BasicBlock::iterator SecondBBI(SecondI);
  BasicBlock *FirstBB = FirstI->getParent();
  BasicBlock *SecondBB = SecondI->getParent();
  MemoryLocation MemLoc;
  if (MemSetInst *MSI = dyn_cast<MemSetInst>(SecondI))
    MemLoc = MemoryLocation::getForDest(MSI);
  else
    MemLoc = MemoryLocation::get(SecondI);

  // Start checking the store-block.
  WorkList.push_back(SecondBB);
  bool isFirstBlock = true;

  // Check all blocks going backward until we reach the load-block.
  while (!WorkList.empty()) {
    BasicBlock *B = WorkList.pop_back_val();

    // Ignore instructions before LI if this is the FirstBB.
    BasicBlock::iterator BI = (B == FirstBB ? FirstBBI : B->begin());

    BasicBlock::iterator EI;
    if (isFirstBlock) {
      // Ignore instructions after SI if this is the first visit of SecondBB.
      assert(B == SecondBB && "first block is not the store block");
      EI = SecondBBI;
      isFirstBlock = false;
    } else {
      // It's not SecondBB or (in case of a loop) the second visit of SecondBB.
      // In this case we also have to look at instructions after SI.
      EI = B->end();
    }
    for (; BI != EI; ++BI) {
      Instruction *I = &*BI;
      if (I->mayWriteToMemory() && I != SecondI) {
        auto Res = AA->getModRefInfo(I, MemLoc);
        if (Res != MRI_NoModRef)
          return false;
      }
    }
    if (B != FirstBB) {
      assert(B != &FirstBB->getParent()->getEntryBlock() &&
          "Should not hit the entry block because SI must be dominated by LI");
      for (auto PredI = pred_begin(B), PE = pred_end(B); PredI != PE; ++PredI) {
        if (!Visited.insert(*PredI).second)
          continue;
        WorkList.push_back(*PredI);
      }
    }
  }
  return true;
}

/// Find all blocks that will unconditionally lead to the block BB and append
/// them to F.
static void FindUnconditionalPreds(SmallVectorImpl<BasicBlock *> &Blocks,
                                   BasicBlock *BB, DominatorTree *DT) {
  for (pred_iterator I = pred_begin(BB), E = pred_end(BB); I != E; ++I) {
    BasicBlock *Pred = *I;
    if (Pred == BB) continue;
    TerminatorInst *PredTI = Pred->getTerminator();
    if (PredTI->getNumSuccessors() != 1)
      continue;

    if (DT->isReachableFromEntry(Pred))
      Blocks.push_back(Pred);
  }
}

/// HandleFree - Handle frees of entire structures whose dependency is a store
/// to a field of that structure.
bool DSE::HandleFree(CallInst *F) {
  bool MadeChange = false;

  MemoryLocation Loc = MemoryLocation(F->getOperand(0));
  SmallVector<BasicBlock *, 16> Blocks;
  Blocks.push_back(F->getParent());
  const DataLayout &DL = F->getModule()->getDataLayout();

  while (!Blocks.empty()) {
    BasicBlock *BB = Blocks.pop_back_val();
    Instruction *InstPt = BB->getTerminator();
    if (BB == F->getParent()) InstPt = F;

    MemDepResult Dep =
        MD->getPointerDependencyFrom(Loc, false, InstPt->getIterator(), BB);
    while (Dep.isDef() || Dep.isClobber()) {
      Instruction *Dependency = Dep.getInst();
      if (!hasMemoryWrite(Dependency, *TLI) || !isRemovable(Dependency))
        break;

      Value *DepPointer =
          GetUnderlyingObject(getStoredPointerOperand(Dependency), DL);

      // Check for aliasing.
      if (!AA->isMustAlias(F->getArgOperand(0), DepPointer))
        break;

      auto Next = ++Dependency->getIterator();

      // DCE instructions only used to calculate that store
      DeleteDeadInstruction(Dependency, *MD, *TLI);
      ++NumFastStores;
      MadeChange = true;

      // Inst's old Dependency is now deleted. Compute the next dependency,
      // which may also be dead, as in
      //    s[0] = 0;
      //    s[1] = 0; // This has just been deleted.
      //    free(s);
      Dep = MD->getPointerDependencyFrom(Loc, false, Next, BB);
    }

    if (Dep.isNonLocal())
      FindUnconditionalPreds(Blocks, BB, DT);
  }

  return MadeChange;
}

/// handleEndBlock - Remove dead stores to stack-allocated locations in the
/// function end block.  Ex:
/// %A = alloca i32
/// ...
/// store i32 1, i32* %A
/// ret void
bool DSE::handleEndBlock(BasicBlock &BB) {
  bool MadeChange = false;

  // Keep track of all of the stack objects that are dead at the end of the
  // function.
  SmallSetVector<Value*, 16> DeadStackObjects;

  // Find all of the alloca'd pointers in the entry block.
  BasicBlock &Entry = BB.getParent()->front();

  const DataLayout &DL = BB.getModule()->getDataLayout();

  // Alyssa added this whole bit here.
  // idea: if a buffer only exists to be passed to a writeonly function
  // but we don't read from it, we can kill all the local stores
  // TODO: why don't we also check other (m)allocs?
  std::set<Instruction *> VeryDeadStores;
  for (Instruction &I : Entry) {
    if (AllocaInst *AI = dyn_cast<AllocaInst>(&I)) {
      bool FoundNonWrite = false;
      DEBUG(dbgs() << "end block DSE now pondering " << *AI << "\n");
      SetVector<Instruction *, SmallVector<Instruction *, 16> > Worklist;
      SmallPtrSet<Instruction *, 16> FoundStores;
      Worklist.insert(AI);
      // Try to find anything which uses this other than just by writing to it..
      for (unsigned int n = 0; n < Worklist.size(); ++n) {
        Instruction *WI = Worklist[n];
        for (Value::use_iterator U = WI->use_begin(), E = WI->use_end(); U != E; ++U) {
          Instruction *UI = dyn_cast<Instruction>(U->getUser());
          // Follow casts/GEPs, but they're not real uses.
          if (dyn_cast<CastInst>(UI) || dyn_cast<GetElementPtrInst>(UI)) {
            Worklist.insert(UI);
            continue;
          }
          if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(UI)) {
            if (U->getOperandNo() == 0) {
              FoundStores.insert(UI);
              continue;
            }
          } else if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(UI)) {
            switch (II->getIntrinsicID()) {
              case Intrinsic::lifetime_start:
              case Intrinsic::lifetime_end:
                // Lifetimes aren't real uses.
                // TODO: Although we could minimise them..?
                continue;
              case Intrinsic::dbg_value:
              case Intrinsic::dbg_declare:
                continue;
              default:
                break;
            }
          }
          if (StoreInst *SI = dyn_cast<StoreInst>(UI)) {
            if (U->getOperandNo() == 1) {
              // this is the pointer target
              FoundStores.insert(UI);
              continue;
            }
          }
          if (auto CS = CallSite(UI)) {
            if (CS.getCalledFunction()) { // see e.g. espresso's lack of prototypes
              // FIXME: is this ok?
              ModRefInfo A = AA->getModRefInfo(CS, AI, getPointerSize(AI, DL, *TLI));
              if (!(A & MRI_Ref))
                continue;
              // FIXME: need to think on this. being kind of paranoid for now.
              auto ArgNo = U->getOperandNo();
              DEBUG(dbgs() << "pondering arg#" << ArgNo << "( " << CS.getCalledFunction()->getName() << ")\n");
              if (CS.doesNotCapture(ArgNo) &&
                  !CS.isByValArgument(ArgNo) &&
                  CS.dataOperandHasImpliedAttr(ArgNo + 1, Attribute::WriteOnly)) {
                continue;
              }
            }
          }
          DEBUG(dbgs() << "nope; operand " << U->getOperandNo() << " of inst " << *UI << " to blame\n");
          FoundNonWrite = true;
          break;
        }
        if (FoundNonWrite) break;
      }
      // .. if we don't find anything, kill all the removable stores.
      if (!FoundNonWrite) {
        DEBUG(dbgs() << "end block DSE killing written-only " << *AI << "\n");
        for (auto Dead : FoundStores)
          VeryDeadStores.insert(Dead);
      }
    }
  }
  for (auto Dead : VeryDeadStores)
    DeleteDeadInstruction(Dead, *MD, *TLI);

  for (Instruction &I : Entry) {
    if (isa<AllocaInst>(&I))
      DeadStackObjects.insert(&I);

    // Okay, so these are dead heap objects, but if the pointer never escapes
    // then it's leaked by this function anyways.
    else if (isAllocLikeFn(&I, TLI) && !PointerMayBeCaptured(&I, true, true))
      DeadStackObjects.insert(&I);
  }

  // Treat byval or inalloca arguments the same, stores to them are dead at the
  // end of the function.
  for (Argument &AI : BB.getParent()->args())
    if (AI.hasByValOrInAllocaAttr())
      DeadStackObjects.insert(&AI);

  // Scan the basic block backwards
  for (BasicBlock::iterator BBI = BB.end(); BBI != BB.begin(); ){
    --BBI;

    // Ignore lifetime ends.
    if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(BBI))
      if (II->getIntrinsicID() == Intrinsic::lifetime_end)
        continue;

    // If we find a store, check to see if it points into a dead stack value.
    if (hasMemoryWrite(&*BBI, *TLI) && isRemovable(&*BBI)) {
      // See through pointer-to-pointer bitcasts
      SmallVector<Value *, 4> Pointers;
      GetUnderlyingObjects(getStoredPointerOperand(&*BBI), Pointers, DL);

      // Stores to stack values are valid candidates for removal.
      bool AllDead = true;
      for (SmallVectorImpl<Value *>::iterator I = Pointers.begin(),
           E = Pointers.end(); I != E; ++I)
        if (!DeadStackObjects.count(*I)) {
          AllDead = false;
          break;
        }

      if (AllDead) {
        Instruction *Dead = &*BBI++;

        DEBUG(dbgs() << "DSE: Dead Store at End of Block:\n  DEAD: "
                     << *Dead << "\n  Objects: ";
              for (SmallVectorImpl<Value *>::iterator I = Pointers.begin(),
                   E = Pointers.end(); I != E; ++I) {
                dbgs() << **I;
                if (std::next(I) != E)
                  dbgs() << ", ";
              }
              dbgs() << '\n');

        // DCE instructions only used to calculate that store.
        DeleteDeadInstruction(Dead, *MD, *TLI, &DeadStackObjects);
        ++NumFastStores;
        MadeChange = true;
        continue;
      }
    }

    // Remove any dead non-memory-mutating instructions.
    if (isInstructionTriviallyDead(&*BBI, TLI)) {
      Instruction *Inst = &*BBI++;
      DeleteDeadInstruction(Inst, *MD, *TLI, &DeadStackObjects);
      ++NumFastOther;
      MadeChange = true;
      continue;
    }

    if (isa<AllocaInst>(BBI)) {
      // Remove allocas from the list of dead stack objects; there can't be
      // any references before the definition.
      DeadStackObjects.remove(&*BBI);
      continue;
    }

    if (auto CS = CallSite(&*BBI)) {
      // Remove allocation function calls from the list of dead stack objects; 
      // there can't be any references before the definition.
      if (isAllocLikeFn(&*BBI, TLI))
        DeadStackObjects.remove(&*BBI);

      // If this call does not access memory, it can't be loading any of our
      // pointers.
      if (AA->doesNotAccessMemory(CS))
        continue;

      // If the call might load from any of our allocas, then any store above
      // the call is live.
      DeadStackObjects.remove_if([&](Value *I) {
        // See if the call site touches the value.
        ModRefInfo A = AA->getModRefInfo(CS, I, getPointerSize(I, DL, *TLI));

//        return A == MRI_ModRef || A == MRI_Ref;
        return A & MRI_Ref;
      });

      // If all of the allocas were clobbered by the call then we're not going
      // to find anything else to process.
      if (DeadStackObjects.empty())
        break;

      continue;
    }

    MemoryLocation LoadedLoc;

    // If we encounter a use of the pointer, it is no longer considered dead
    if (LoadInst *L = dyn_cast<LoadInst>(BBI)) {
      if (!L->isUnordered()) // Be conservative with atomic/volatile load
        break;
      LoadedLoc = MemoryLocation::get(L);
    } else if (VAArgInst *V = dyn_cast<VAArgInst>(BBI)) {
      LoadedLoc = MemoryLocation::get(V);
    } else if (MemTransferInst *MTI = dyn_cast<MemTransferInst>(BBI)) {
      LoadedLoc = MemoryLocation::getForSource(MTI);
    } else if (!BBI->mayReadFromMemory()) {
      // Instruction doesn't read memory.  Note that stores that weren't removed
      // above will hit this case.
      continue;
    } else {
      // Unknown inst; assume it clobbers everything.
      break;
    }

    // Remove any allocas from the DeadPointer set that are loaded, as this
    // makes any stores above the access live.
    RemoveAccessedObjects(LoadedLoc, DeadStackObjects, DL);

    // If all of the allocas were clobbered by the access then we're not going
    // to find anything else to process.
    if (DeadStackObjects.empty())
      break;
  }

  return MadeChange;
}

/// RemoveAccessedObjects - Check to see if the specified location may alias any
/// of the stack objects in the DeadStackObjects set.  If so, they become live
/// because the location is being loaded.
void DSE::RemoveAccessedObjects(const MemoryLocation &LoadedLoc,
                                SmallSetVector<Value *, 16> &DeadStackObjects,
                                const DataLayout &DL) {
  const Value *UnderlyingPointer = GetUnderlyingObject(LoadedLoc.Ptr, DL);

  // A constant can't be in the dead pointer set.
  if (isa<Constant>(UnderlyingPointer))
    return;

  // If the kill pointer can be easily reduced to an alloca, don't bother doing
  // extraneous AA queries.
  if (isa<AllocaInst>(UnderlyingPointer) || isa<Argument>(UnderlyingPointer)) {
    DeadStackObjects.remove(const_cast<Value*>(UnderlyingPointer));
    return;
  }

  // Remove objects that could alias LoadedLoc.
  DeadStackObjects.remove_if([&](Value *I) {
    // See if the loaded location could alias the stack location.
    MemoryLocation StackLoc(I, getPointerSize(I, DL, *TLI));
    return !AA->isNoAlias(StackLoc, LoadedLoc);
  });
}
