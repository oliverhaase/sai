package sai.bytecode.instruction

import cg.ConnectionGraph
import sai.bytecode.Method
import vm.Frame

class ExitPoint(method: Method) extends Instruction(null, null, method) {

  override def next: Instruction = method.lastInstruction

  override def prev: Instruction =
    throw new RuntimeException("this value is not supposed to be used")

  override def successors: List[Instruction] = List()

  override def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = frame

  override def lineNumber: Int = method.lastInstruction.lineNumber

  override def compare(that: Instruction): Int = 1

  override def toString = "exit point"
}