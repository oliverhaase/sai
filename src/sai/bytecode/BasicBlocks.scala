package bytecode

import sai.bytecode.Method
import sai.bytecode.instruction.ControlFlowInstruction
import sai.bytecode.instruction.ExitPoint
import sai.bytecode.instruction.EntryPoint

object BasicBlocks {

  def apply(method: Method): List[BasicBlock] = {

    val leaders = method.instructions.flatMap {
      case i: EntryPoint => Some(i)
      case i: ExitPoint => Some(i)
      case i: ControlFlowInstruction => i.next :: i.successors
      case _ => None
    }.distinct.sorted

    val basicBlocks =
      for (leader <- leaders)
        yield new BasicBlock(method, leader)

    merge(basicBlocks)
  }


  private def merge(blocks: List[BasicBlock]): List[BasicBlock] = {

    // we merge blocks by dropping those not necessary
    val constraints = List(
      dropSingleExitPoint,
      dropSuccessorOfFinally,
      dropFinallyAfterTry
    )

    blocks.filterNot { block =>
      val drop = constraints.exists(_.lift(block).getOrElse(false))
      drop
    }
  }

  type DropConstraint = PartialFunction[BasicBlock, Boolean]

  private def dropSingleExitPoint: DropConstraint = {
    case BasicBlock(_, leader: ExitPoint) if leader.predecessors.size == 1 => true
  }

  private def dropSuccessorOfFinally: DropConstraint = {
    case BasicBlock(m, leader) if leader.predecessors.exists(m.exceptionInfo.isThrowInFinallyBlock) => true
  }

  private def dropFinallyAfterTry: DropConstraint = {
    case BasicBlock(m, leader) if m.exceptionInfo.isFinallyLeader(leader) =>
      leader.predecessors match {
        case pred :: Nil if m.exceptionInfo.isLastTryInstruction(pred) => true
        case _ => false
      }
  }

}
