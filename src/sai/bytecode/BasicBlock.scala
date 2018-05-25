package bytecode

import scala.annotation.tailrec

import cg.ConnectionGraph
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.ExitPoint
import sai.bytecode.instruction.ControlFlowInstruction
import vm.Frame

class BasicBlock(method: Method, private val leader: Instruction) {

  def lineRange = Range.inclusive(leader.lineNumber, lastInstruction.lineNumber)

  override def toString: String = lineRange.toString()

  def predecessors =
    for (basicBlock <- method.basicBlocks if basicBlock.successors.contains(this))
      yield basicBlock

  def successors =
    for (basicBlock <- method.basicBlocks if successorLeaders.contains(basicBlock.leader))
      yield basicBlock

  private def instructions = {
    val succLeaders = successorLeaders

    @tailrec
    def collectInstructions(current: Instruction, collection: List[Instruction]): List[Instruction] = {
      current.successors match {
        case next :: Nil if next.isLastInstructionInsideTryBlock =>
          collection :+ current :+ next
        case next :: Nil if !succLeaders.contains(next) =>
          collectInstructions(next, collection :+ current)
        case _ =>
          collection :+ current
      }
    }
    collectInstructions(leader, Nil)
  }

  private def successorLeaders = {
    val leaders =
      for (basicBlock <- method.basicBlocks)
        yield basicBlock.leader

    @tailrec
    def findNextLeaders(instruction: Instruction): List[Instruction] = {
      instruction match {
        case _: ControlFlowInstruction =>
          instruction.successors
        case _ if leaders.contains(instruction) =>
          List(instruction)
        case _ if instruction.isFirstInstructionInsideTryBlock =>
          List(instruction)
        case _ if instruction.isLastInstructionInsideTryBlock =>
          if (instruction.next.isInstanceOf[ControlFlowInstruction]) {
            method.getCatchInstructions(instruction) ::: instruction.next.successors
          } else method.getCatchInstructions(instruction)
        case _: ExitPoint =>
          Nil
        case _ =>
          findNextLeaders(instruction.next)
      }
    }
    findNextLeaders(leader.next).distinct
  }

  private def lastInstruction = instructions.last

  def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = {
    val inState = inStates.reduce(_ merge _)
    frame
  }

}
