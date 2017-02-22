package sai.bytecode.instruction

import sai.bytecode.Method

class EntryPoint(method: Method) extends Instruction(null, null, method) {
   override def toString = "entry point"
}