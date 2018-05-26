package bytecode

import scala.annotation.tailrec

import cg.ConnectionGraph
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.ExitPoint
import sai.bytecode.instruction.ControlFlowInstruction
import vm.Frame

class BasicBlock(method: Method, private val leader: Instruction) {

  def lineRange = leader.lineNumber to lastInstruction.lineNumber

  override def toString: String = s"${method.name} ${lineRange.toString()}"

  def predecessors: List[BasicBlock] =
    for (basicBlock <- method.basicBlocks if basicBlock.successors.contains(this))
      yield basicBlock

  def successors: List[BasicBlock] =
    for (basicBlock <- method.basicBlocks if successorLeaders.contains(basicBlock.leader))
      yield basicBlock

  def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = {
    val inState = inStates.reduce(_ merge _)
    frame
  }

  lazy val lastInstruction: Instruction = {
    val leaders = method.basicBlocks.map(_.leader)
    @tailrec
    def findLast(instruction: Instruction): Instruction = instruction match {
      case i: ExitPoint => i
      case i: ControlFlowInstruction => i
      case i if i.isLastTryInstruction => i
      case i if leaders.contains(i.next) => i
      case i => findLast(i.next)
    }
    findLast(leader.next)
  }

  private def instructions: List[Instruction] = {
    val last = lastInstruction
    @tailrec
    def collectUntilLast(i: Instruction, instructions: List[Instruction]): List[Instruction] = {
      if (i == last) instructions :+ i
      else collectUntilLast(i.next, instructions :+ i)
    }
    collectUntilLast(leader, Nil)
  }

  private def successorLeaders: List[Instruction] = {
    lastInstruction match {
      case _: ExitPoint => Nil
      case i: ControlFlowInstruction => i.successors
      case i if i.isLastTryInstruction =>
        // get all catch leaders (including the finally leader if there is one)
        val catchLeaders = method.findCatchLeaders(i)
        // check the successor of the last try instruction
        i.next match {
          case next: ControlFlowInstruction =>
            // it is a branch instruction, so there is no finally block!
            // -> add the targets of the branch instruction to the leader list
            next.successors ::: catchLeaders
          case _ =>
            // it is not a branch instruction, so there is a finally block!
            // -> the finally leader is already also in the catch leaders list
            catchLeaders
        }
      case i => i.successors
    }
  }

}
