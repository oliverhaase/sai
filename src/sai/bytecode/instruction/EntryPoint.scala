package sai.bytecode.instruction

import cg.ConnectionGraph
import sai.bytecode.Method
import vm.Frame

class EntryPoint(method: Method) extends Instruction(null, null, method) {

  override def next: Instruction = method.firstInstruction

  override def successors: List[Instruction] = List(next)

  override def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = frame

  override def toString = "entry point"

}