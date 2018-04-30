package sai.bytecode.instruction

import sai.bytecode.Method

class ExitPoint(method: Method) extends Instruction(null, null, method) {
   override def next: Instruction =
      throw new RuntimeException("this value is not supposed to be used")

   override def successors: List[Instruction] = List()

   override def toString = "exit point"
}