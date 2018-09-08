package bytecode

import sai.bytecode.Method
import sai.bytecode.instruction.{ExitPoint, Instruction}
import vm.Frame

import scala.annotation.tailrec

class BasicBlock(val method: Method, val leader: Instruction) {

  def lineRange = leader.lineNumber to lastInstruction.lineNumber

  override def toString: String = StringContext("", " ", "").s(method.name, lineRange.toString())

  def successors: List[BasicBlock] =
    for (basicBlock <- method.controlFlowGraph if successorLeaders.contains(basicBlock.leader))
      yield basicBlock

  def predecessors: List[BasicBlock] =
    for (basicBlock <- method.controlFlowGraph if basicBlock.successors.contains(this))
      yield basicBlock

  def interpret(inFrame: Frame): List[Frame] = {
    instructions.foldLeft(inFrame :: Nil)((frames, i) => frames.flatMap(i.interpret))
  }

  private lazy val lastInstruction: Instruction = {
    val leaders =
      for (basicBlock <- method.controlFlowGraph)
        yield basicBlock.leader

    @tailrec
    def findLast(instruction: Instruction): Instruction = instruction match {
      case ep: ExitPoint => ep
      case i if i.successors.exists(leaders.contains) => i
      case i =>
        assert(i.successors.lengthCompare(1) == 0)
        findLast(i.successors.head)
    }
    findLast(leader)
  }

  def instructions: List[Instruction] = {
    val last = lastInstruction
    @tailrec
    def collectUntilLast(i: Instruction, instructions: List[Instruction]): List[Instruction] = {
      if (i == last) instructions :+ i
      else collectUntilLast(i.next, instructions :+ i)
    }
    collectUntilLast(leader, Nil)
  }

  private def successorLeaders: List[Instruction] = lastInstruction.successors

}

object BasicBlock {
  def unapply(arg: BasicBlock): Option[(Method, Instruction)] = Some(arg.method, arg.leader)
}