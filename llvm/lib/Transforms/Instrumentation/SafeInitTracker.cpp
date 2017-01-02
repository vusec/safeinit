//===- SafeInitTracker.cpp -------------------------------------------- ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file checks for oversized zero-initializations and warns about them.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
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
using namespace llvm;

#define DEBUG_TYPE "safeinittracker"

static cl::opt<unsigned> MaxHeapInitSize ("ZEROINITCHECKER_MAXHEAPINITSIZE", cl::desc("Size which is considered excessive for zero-initializing heap"), cl::init(32 * 1024));
static cl::opt<unsigned> MaxStackInitSize ("ZEROINITCHECKER_MAXSTACKINITSIZE", cl::desc("Size which is considered excessive for zero-initializing stack"), cl::init(2 * 1024));

STATISTIC(FinalStackZeroInitCounter, "Counts number of stackzeroinit memsets which weren't removed");
STATISTIC(FinalHeapZeroInitCounter, "Counts number of heapzeroinit memsets which weren't removed");

namespace {
  struct SafeInitTracker : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    SafeInitTracker() : FunctionPass(ID) {}

    unsigned stackMDKind;
    unsigned heapMDKind;

    const char *getPassName() const { return "Zero-Initialization Checker"; }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
    }

    bool runOnFunction(Function &F) override;
  };
}

INITIALIZE_PASS(SafeInitTracker, "safeinittracker",
    "SafeInitTracker: hack to report large uninited variables.",
    false, false)

bool SafeInitTracker::runOnFunction(Function &F) {
  Module *M = F.getParent();
  LLVMContext &C = M->getContext();

  stackMDKind = C.getMDKindID("stackzeroinit");
  heapMDKind = C.getMDKindID("heapzeroinit");

  for (Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I) {
      auto stackMD = I->getMetadata(stackMDKind);
      auto heapMD = I->getMetadata(heapMDKind);

      // update counter
      if (stackMD)
        FinalStackZeroInitCounter++;
      else if (heapMD)
        FinalHeapZeroInitCounter++;
      else
        continue;

      // see if we have a memset
      MemIntrinsic *II = dyn_cast<MemIntrinsic>(I);
      if (!II || II->getIntrinsicID() != Intrinsic::memset)
        continue;
      Value *length = II->getLength();
      if (ConstantInt *CI = dyn_cast<ConstantInt>(length)) {
        uint64_t value = CI->getValue().getLimitedValue();
        // TODO: nice debug info instead?
        if (stackMD && value >= MaxStackInitSize)
          errs() << "Warning: inited stack alloc " << *II->getDest() << " (in " << F.getName() << ") is excessively large (" << value << " bytes)\n";
        if (heapMD && value >= MaxHeapInitSize)
          errs() << "Warning: inited heap alloc " << *II->getDest() << " (in " << F.getName() << ") is excessively large (" << value << "bytes)\n";
      }
    }
  }

  return false;
}

char SafeInitTracker::ID = 0;

