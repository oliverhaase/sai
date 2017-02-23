package sai.bytecode.instruction

import sai.bytecode.Method

class EntryPoint(method: Method) extends Instruction(null, null, method) {
   override def next: Instruction = method firstInstruction
   
   override def successors: List[Instruction] = List(next)
  
   override def toString = "entry point"
}