//===-- X86FrameInit.cpp - X86 Frame Initialization -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Initialization (and clearing) of stack frames on X86.
//
//===----------------------------------------------------------------------===//

#include "X86.h"
#include "X86FrameLowering.h"
#include "X86InstrInfo.h"
#include "X86MachineFunctionInfo.h"
#include "X86Subtarget.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/Passes.h"
using namespace llvm;

#define DEBUG_TYPE "x86frameinit"

static cl::opt<bool> EnableFrameInit( "enable-frame-init", cl::Hidden, cl::init(false),
    cl::desc("Clear stack frames in function prologues"));
static cl::opt<bool> EnableFrameClear( "enable-frame-clear", cl::Hidden, cl::init(false),
    cl::desc("Clear stack frames in function epilogues"));

namespace {
struct X86FrameInit : public llvm::MachineFunctionPass {
  static char ID;
  X86FrameInit() : llvm::MachineFunctionPass(ID) { }

  const X86Subtarget *STI;
  const X86InstrInfo *TII;
  const X86RegisterInfo *TRI;
  const X86FrameLowering *X86FL;

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties().set(
        MachineFunctionProperties::Property::AllVRegsAllocated);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  const char *getPassName() const override { return "X86 Frame Clearing"; }

  SmallVector<unsigned, 1> PrologueBlocks;
  SmallVector<unsigned, 4> EpilogueBlocks;
};
}

FunctionPass *llvm::createX86FrameInitPass() {
  return new X86FrameInit();
}

bool X86FrameInit::runOnMachineFunction(MachineFunction &MF) {
  if (!(EnableFrameInit || EnableFrameClear))
    return false;

  PrologueBlocks.clear();
  EpilogueBlocks.clear();

  STI = &static_cast<const X86Subtarget &>(MF.getSubtarget());
  TII = STI->getInstrInfo();
  TRI = STI->getRegisterInfo();
  X86FL = STI->getFrameLowering();

  bool hasFP = X86FL->hasFP(MF);

  // also no Windows support (no funclets! no stack probing!) and many other limitations
  assert(X86FL->Is64Bit && "only 64-bit mode is supported");
  assert(!MF.getFunction()->hasFnAttribute(Attribute::Naked) && "naked functions not supported");

  MachineFrameInfo *MFI = MF.getFrameInfo();
  X86MachineFunctionInfo *X86FI = MF.getInfo<X86MachineFunctionInfo>();

  // PEI::calculateFrameObjectOffsets always adds abs(TFI.getOffsetOfLocalArea) which is -8
  // do we always overestimate by 8, then? TODO: just ignoring this for now, seems fine, SEE ALSO COMMENT BELOW
  /*unsigned StackSize = MFI->getStackSize() + 8;*/
  unsigned StackSize = MFI->getStackSize();

  // we don't yet add a slot after checking getRestoreBasePointer() for SJLJ exceptions
  assert(!X86FI->getRestoreBasePointer());

  // We can't (easily) work out how much of the red zone is used.
  // -> Alyssa hacked this into X86FI.
  if (X86FI->getUsesRedZone()) {
    // TODO: I'm distrustful of the LLVM code for this. And so also this :(
    unsigned oldSize = X86FI->getPreRedZoneSize();
    dbgs() << "red zone use in " << MF.getFunction()->getName() << " was " << oldSize << "\n";

    if (oldSize >= 128)
      StackSize += 128; // entire red zone used
    else
      StackSize += oldSize; // partial red zone used
  }

  // sp-8 points to:
  // [spilled fp] (pushed)
  // [spilled GPRs] (pushed; this is getCalleeSavedFrameSize())
  // <alignment>
  // [spilled non-GPRs]

  // We don't know whether any realignment will happen, so we just assume the worst-case.
  // FIXME: X86FL->calculateMaxStackAlign(MF) is private :-/ (but this is OK for our purposes)
  if (MFI->getLocalFrameMaxAlign() > 16) {
    dbgs() << "need to adjust for frame max align in " << MF.getFunction()->getName() << ", which is " << MFI->getLocalFrameMaxAlign() << "\n";
    StackSize += MFI->getLocalFrameMaxAlign();
  }

  // We don't need to clear spilled registers.
  // The frame lowering always helpfully puts them first, too.
  unsigned SpilledStackSize = 0;
  int MinimumSpilledOffset = -8;
  if (hasFP) {
  //  SpilledStackSize += TRI->getSlotSize();
    MinimumSpilledOffset = -16; // meh, but works
  }
  for (int n = MFI->getObjectIndexBegin(); n < 0; ++n) {
    if (MFI->isSpillSlotObjectIndex(n)) {
//      SpilledStackSize += MFI->getObjectSize(n);
      int offset = MFI->getObjectOffset(n);
      assert(offset < 0);
      if (offset < MinimumSpilledOffset)
        MinimumSpilledOffset = offset;
      assert(MFI->getObjectSize(n)); // never dynamic! (getObjectOffset asserts non-dead)
    }
  }

  // TODO: make sure we never have to care about MFI->getOffsetAdjustment()

  // Sometimes there are normal allocations overlapping, so we need to adjust. TODO: wtf
  for (int n = MFI->getObjectIndexBegin(); n < 0; ++n) {
    if (!MFI->getObjectSize(n)) continue; // dynamic allocation
    if (!MFI->isSpillSlotObjectIndex(n)) {
      if (MFI->getObjectOffset(n) >= MinimumSpilledOffset) {
        if (MFI->getObjectOffset(n) >= 0) {
          // TODO: is this some kind of tail call thing?
          // e.g. Perl_do_open (perlbench) has one at offset 16
          dbgs() << "ignoring (?!) non-spill at offset " << MFI->getObjectOffset(n) << "\n";
        } else {
          dbgs() << "alas, non-spill at offset " << MFI->getObjectOffset(n) << ", adjusting\n";
          MinimumSpilledOffset = MFI->getObjectOffset(n); // make sure this object is included
        }
      }
    }
  }

  // this is the only way to calculate this
  SpilledStackSize = -MinimumSpilledOffset - 8; // TODO: the -8 is to adjust for the PEI::calculateFrameObjectOffsets, see comment above

  dbgs() << MF.getFunction()->getName() << " has real stack size " << MFI->getStackSize() << " and we think " << StackSize << " and min offset is " << MinimumSpilledOffset << " and spilled stack size is " << SpilledStackSize << "\n";
  if (X86FI->getUsesRedZone()) dbgs() << "(uses red zone)\n";

  assert(SpilledStackSize <= StackSize && "spilled stack content size can't exceed stack size");

  unsigned StackSizeToClear = StackSize - SpilledStackSize;

  dbgs() << "going to clear " << StackSizeToClear << " bytes (of " << StackSize << ") in " << MF.getFunction()->getName() << " (spilled size is " << SpilledStackSize << ")\n";

  if (!StackSizeToClear)
    return false;

  // esp-8 is aligned on function entry
  // we want to clear an aligned number of 16-byte chunks
  bool UnalignedClearFirst = false;
  unsigned ClearFromOffset = SpilledStackSize; // i.e. negative, from esp

  // TODO: this happens for dealII, can it also happen with smaller sizes?
  if (StackSizeToClear == 4) {
    StackSizeToClear = 8;
  }

  // Is the start offset going to write beyond our stack if we align it?
  if (ClearFromOffset == 0) {
    // If so, manually clear the first 8 bytes.
    UnalignedClearFirst = true;
    ClearFromOffset += 8;
    assert(StackSizeToClear >= 8);
    StackSizeToClear -= 8;
  }

  // Is the start offset otherwise unaligned?
  if (ClearFromOffset % 16 != 8) {
    assert(ClearFromOffset >= 8);
    // If so, just clear an extra 8 bytes.
    ClearFromOffset -= 8;
    StackSizeToClear += 8;
  }

  assert(ClearFromOffset % 16 == 8);

  // We always clear in 16-byte chunks, to keep this simple.
  StackSizeToClear = alignTo(StackSizeToClear, 16);
  assert(StackSizeToClear % 16 == 0);
  StackSizeToClear /= 16;

  // notes:
  // stack starts at MFI->getOffsetOfLocalArea() which is always -8 for us
  // alignment is MFI->getStackAlignment() which is usually(?) 16 for us, there will be no skew

  // TODO: using offsets beyond -0x80 add 3 bytes to each movaps instruction
  //        if we have one loop then that's 12 bytes, so in some cases (1 loop plus >1 before-loop inst)
  //        it's more efficient for code-size if we adjust the stack anyway (14 bytes)
  //        (but this is really micro-optimizing)

  if (EnableFrameInit) {
    PrologueBlocks.push_back(MF.front().getNumber());

    for (auto MBBN : PrologueBlocks) {
      auto MBB = MF.getBlockNumbered(MBBN);

      unsigned CountInFirstBlock = StackSizeToClear % 4;
      unsigned LoopCount = StackSizeToClear / 4;
      MachineBasicBlock *FirstBB = MBB;
      MachineBasicBlock *LoopBB = MBB;

      if (StackSizeToClear > 4) {
        FirstBB = MF.CreateMachineBasicBlock();
        LoopBB = MF.CreateMachineBasicBlock();
        MF.push_front(LoopBB);
        MF.push_front(FirstBB);
        MBB = MF.getBlockNumbered(MBBN); // Probably the pointer changed.
        FirstBB->addSuccessor(LoopBB);

        // The loop header sets R11 (only always-available scratch register).
        if (LoopCount > 1)
          BuildMI(*FirstBB, FirstBB->end(), DebugLoc(), TII->get(X86::MOV32ri), X86::R11).addImm(LoopCount);

        // The loop itself decrements R11 (by 1) and RSP (by 16*4), until R11 is zero.
        LoopBB->addSuccessor(LoopBB);
        LoopBB->addSuccessor(MBB);
        if (LoopCount > 1) {
          BuildMI(*LoopBB, LoopBB->end(), DebugLoc(), TII->get(X86::SUB64ri8), X86::RSP).addReg(X86::RSP).addImm(16 * 4);
          BuildMI(*LoopBB, LoopBB->end(), DebugLoc(), TII->get(X86::SUB64ri8), X86::R11).addReg(X86::R11).addImm(1);
          BuildMI(*LoopBB, LoopBB->end(), DebugLoc(), TII->get(X86::JNE_1)).addMBB(LoopBB);
        }
      } else {
        LoopCount = 0;
        CountInFirstBlock = StackSizeToClear;
      }

      auto StackRestorePoint = MBB->begin();

      // We have to adjust RSP to skip over the bit we don't clear.
      // (We do this just before we enter the loop.)
      int FirstStackAdjustment = ClearFromOffset;
      if (CountInFirstBlock) {
        FirstStackAdjustment += 16 * CountInFirstBlock;
      }
      if (LoopCount > 1) {
        auto SAI = BuildMI(*FirstBB, FirstBB->begin(), DebugLoc(), TII->get(X86::SUB64ri32), X86::RSP).addReg(X86::RSP).addImm(FirstStackAdjustment);
        SAI->getOperand(3).setIsDead(); // eflags is dead
      }

      for (unsigned n = 0; n < CountInFirstBlock + 4; ++n) {
        MachineBasicBlock *BB;
        int Disp;
        if (n < CountInFirstBlock) {
          BB = FirstBB;
          Disp = -16*(n+1) - ClearFromOffset; // RSP wasn't adjusted yet.
        } else {
          if (!LoopCount)
            break;
          BB = LoopBB;
          Disp = -16*(n+1-CountInFirstBlock);
          // we don't bother adjusting RSP in this case
          if (LoopCount == 1)
            Disp = -16*(n+1-CountInFirstBlock) - FirstStackAdjustment;
        }

        // mod r/m is sort of documented in lib/Target/X86/X86InstrBuilder.h
        //BuildMI(*BB, BB->begin(), DebugLoc(), TII->get(X86::MOVUPSmr))
        BuildMI(*BB, BB->begin(), DebugLoc(), TII->get(X86::MOVAPSmr)) // movaps isn't necessary, but just to be sure we didn't screw up the alignment..
          .addReg(X86::RSP) // base
          .addImm(1) // scale
          .addReg(0) // index
          .addImm(Disp) // displacement
          .addReg(0) // segment
          // src
          .addReg(X86::XMM8)
          ;
      }

      // FIXME: use xmm7 instead (no rex prefix) if it's available
      if (StackSizeToClear)
        BuildMI(*FirstBB, FirstBB->begin(), DebugLoc(), TII->get(X86::XORPSrr), X86::XMM8).addReg(X86::XMM8).addReg(X86::XMM8);

      if (UnalignedClearFirst) {
        BuildMI(*FirstBB, FirstBB->begin(), DebugLoc(), TII->get(X86::MOV64mi32))
          .addReg(X86::RSP) // base
          .addImm(1) // scale
          .addReg(0) // index
          .addImm(-ClearFromOffset) // displacement
          .addReg(0) // segment
          // src
          .addImm(0)
          ;
      }

      if (LoopCount > 1) {
        // Restore the stack pointer.
        int RestoreStackOffset = (16 * StackSizeToClear) + ClearFromOffset;
        // This only works if there's no FP (maybe we can do better someday).
        // FIXME: check if this is ok
        RestoreStackOffset -= X86FL->mergeSPUpdates(*MBB, StackRestorePoint, false);
        if (RestoreStackOffset) {
          auto SRI = BuildMI(*MBB, StackRestorePoint, DebugLoc(), TII->get(X86::ADD64ri32),
            X86::RSP).addReg(X86::RSP).addImm(RestoreStackOffset);
          SRI->getOperand(3).setIsDead(); // eflags is dead
        }
      }
    }
  }

  if (EnableFrameClear) {
    for (MachineBasicBlock &MBB : MF) {
      if (MBB.isReturnBlock())
        EpilogueBlocks.push_back(MBB.getNumber());
    }

    // TODO
  }

  return true;
}

char X86FrameInit::ID = 0;
