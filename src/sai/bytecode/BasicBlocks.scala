package bytecode

import sai.bytecode.Method
import sai.bytecode.instruction.Instruction

object BasicBlocks {

  def apply(method: Method): List[BasicBlock] = {
    for (instruction <- method.instructions if isLeader(instruction))
      yield new BasicBlock(method, leader = instruction)
  }

  private def isLeader(i: Instruction) = {
    i.predecessors.flatMap(_.successors) != ::(i, Nil)
  }

}
