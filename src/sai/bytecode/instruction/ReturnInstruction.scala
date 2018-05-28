package bytecode.instruction

import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction

class ReturnInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle,
    cpg: ConstantPoolGen, method: Method) extends Instruction(bcelInstruction, cpg, method) {

  override def next = method.exitPoint

}
