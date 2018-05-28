package bytecode

import scala.annotation.tailrec

import bytecode.instruction.ReturnInstruction
import cg.ConnectionGraph
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.ExitPoint
import sai.bytecode.instruction.ControlFlowInstruction
import sai.bytecode.instruction.EntryPoint
import vm.Frame

class BasicBlock(method: Method, val leader: Instruction) {

  def lineRange = leader.lineNumber to lastInstruction.lineNumber

  override def toString: String = s"${method.name} ${lineRange.toString()}"

  def successors: List[BasicBlock] =
    for (basicBlock <- method.controlFlowGraph if successorLeaders.contains(basicBlock.leader))
      yield basicBlock

  def predecessors: List[BasicBlock] =
    for (basicBlock <- method.controlFlowGraph if basicBlock.successors.contains(this))
      yield basicBlock

  def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = {
    val inState = inStates.reduce(_ merge _)
    frame
  }

  private lazy val lastInstruction: Instruction = {
    val leaders = method.controlFlowGraph.map(_.leader)
    @tailrec
    def findLast(instruction: Instruction): Instruction = instruction match {
      case ep: ExitPoint => ep
      case i if (i.next :: i.successors).exists(leaders.contains) => i
      case i => findLast(i.next)
    }
    findLast(leader)
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

  private def successorLeaders: List[Instruction] = lastInstruction.successors

}

object BasicBlocks {

  def apply(method: Method): List[BasicBlock] = {
    var leaders = method.instructions.flatMap {
      case i: EntryPoint => Some(i)
      case i: ControlFlowInstruction => i.next :: i.successors
      case _ => None
    }.distinct.sortBy(_.pc)

    leaders = leaders :+ method.exitPoint

    for (leader <- leaders)
      yield new BasicBlock(method, leader)
  }

}