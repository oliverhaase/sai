package sai.bytecode.instruction

import sai.bytecode.Method

class ExitPoint(method: Method) extends Instruction(null, null, method) {
   override def toString = "exit point"
}