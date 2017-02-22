package sai.bytecode.instruction

import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method

class ControlFlowInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, 
  cpg: ConstantPoolGen, method: Method) extends Instruction(bcelInstruction, cpg, method) {
      
}