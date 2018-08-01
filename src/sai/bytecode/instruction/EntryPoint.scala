package sai.bytecode.instruction

import sai.bytecode.Method

class EntryPoint(method: Method) extends Instruction(null, null, method) {

  override def id: String = buildId("entry")

  override def prev: Instruction = null

  override def next: Instruction = method.firstInstruction

  override def successors: List[Instruction] = List(next)

  override def lineNumber: Int = method.firstInstruction.lineNumber - 1

  override def compare(that: Instruction): Int = -1

  override def toString = "entry point"

}