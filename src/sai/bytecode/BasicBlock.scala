package bytecode

import scala.annotation.tailrec

import sai.bytecode.Method
import sai.bytecode.instruction.Instruction

class BasicBlock(method: Method, leader: Instruction) {

  def predecessors =
    for (basicBlock <- method.basicBlocks if basicBlock.successors.contains(this))
      yield basicBlock

  def successors =
    for (successorLeader <- instructions.last.successors)
      yield new BasicBlock(method, successorLeader)

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

}
