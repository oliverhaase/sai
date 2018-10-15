package sai.bytecode.instruction

import sai.bytecode.Method
import vm.Frame

class ExitPoint(method: Method) extends Instruction(null, null, method) {

  override def id: String = buildId("exit")

  override def next: Instruction = null

  override def prev: Instruction =
    throw new RuntimeException("this value is not supposed to be used")

  override def successors: List[Instruction] = List()

  override def lineNumber: Int = method.lastInstruction.lineNumber

  override def interpret(frame: Frame): List[Frame] = frame :: Nil

  override def compare(that: Instruction): Int = 1

  override def toString = s"$id exit point"
}