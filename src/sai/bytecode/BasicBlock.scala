package bytecode

import scala.annotation.tailrec

import cg.ConnectionGraph
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction
import vm.Frame

class BasicBlock(method: Method, leader: Instruction) {

  def startLine = leader.lineNumber
  def lines = instructions.map(_.lineNumber).distinct.sorted

  override def toString: String = {
    s"$startLine {$lines}"
  }

  def predecessors =
    for (basicBlock <- method.basicBlocks if basicBlock.successors.contains(this))
      yield basicBlock

  def successors = Nil

  private def instructions = {
    @tailrec
    def collectInstructions(current: Instruction, collection: List[Instruction]): List[Instruction] = {
      current.successors match {
        case next :: Nil => collectInstructions(next, collection :+ current)
        case _ => collection :+ current
      }
    }
    collectInstructions(leader, Nil)
  }

  def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = {
    val inState = inStates.reduce(_ merge _)
    frame
  }

}
