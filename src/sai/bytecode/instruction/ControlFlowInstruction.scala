package sai.bytecode.instruction

import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method

class ControlFlowInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, 
  cpg: ConstantPoolGen, method: Method) extends Instruction(bcelInstruction, cpg, method) {
   
  override def successors: List[Instruction] = bcelInstruction.getInstruction match {
    case i: org.apache.bcel.generic.GotoInstruction => 
      List(lookupInstruction(i.getTarget))
    case i: org.apache.bcel.generic.IfInstruction => 
      List(lookupInstruction(i.getTarget), next)
    case _: org.apache.bcel.generic.JsrInstruction =>
      throw new RuntimeException("jsr instruction is not supported")
    case i: org.apache.bcel.generic.Select =>
        val targetList = 
          for ( target <- i.getTargets.toList ) 
            yield lookupInstruction(target)
        next :: targetList
    case _ => List(next)
  }

}