package sai.bytecode.instruction

import sai.bytecode.Method

class EntryPoint(method: Method) extends Instruction(null, null, method) {
   override def next: Option[Instruction] = Some(method firstInstruction)
  
   override def toString = "entry point"
}