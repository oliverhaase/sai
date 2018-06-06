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


  private[this] def merge(blocks: List[BasicBlock]): List[BasicBlock] = {

    // we merge blocks by dropping those not necessary
    val constraints = List(
      dropFinallyBlock,
      dropSingleSuccessorBlock
    )

    blocks.filterNot { block =>
      val drop = constraints.exists(constraint => constraint.applyOrElse(block, (_: BasicBlock) => false))
      drop
    }
  }

  type DropConstraint = PartialFunction[BasicBlock, Boolean]

  private[this] def dropFinallyBlock: DropConstraint = {
    case BasicBlock(method, leader) if method.exceptionInfo.isFinallyLeader(leader) =>
      true
  }

  private[this] def dropSingleSuccessorBlock: DropConstraint = {
    case BasicBlock(method, leader) if method.exceptionInfo.isCatchLeader(leader) =>
      false
    case BasicBlock(_, leader) if leader.predecessors.size != 1 =>
      false
    case BasicBlock(_, leader) if leader.predecessors.head.successors.size == 1 =>
      true
  }

}
