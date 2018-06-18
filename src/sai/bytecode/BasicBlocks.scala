package bytecode

import sai.bytecode.Method
import sai.bytecode.instruction.ControlFlowInstruction
import sai.bytecode.instruction.ExitPoint
import sai.bytecode.instruction.EntryPoint

object BasicBlocks {

  def apply(method: Method): List[BasicBlock] = {

    val leaders = method.instructions.collect {
      case i: EntryPoint => i
      case i: ExitPoint => i
      case i if (i.prev :: i.predecessors).exists(_.isInstanceOf[ControlFlowInstruction]) => i
      case i if method.exceptionInfo.isInToList(i) => i
    }.distinct.sorted

    val basicBlocks =
      for (leader <- leaders)
        yield new BasicBlock(method, leader)

    merge(basicBlocks)
  }


  private[this] def merge(blocks: List[BasicBlock]): List[BasicBlock] = {

    // we merge blocks by dropping those not necessary
    val constraints = List(
      dropSingleSuccessorBlock
    )

    blocks.filterNot { block =>
      val drop = constraints.exists(constraint => constraint.applyOrElse(block, (_: BasicBlock) => false))
      drop
    }
  }

  type DropConstraint = PartialFunction[BasicBlock, Boolean]

  private[this] def dropSingleSuccessorBlock: DropConstraint = {
    case BasicBlock(_, leader) if leader.predecessors.size != 1 =>
      false
    case BasicBlock(_, leader) if leader.predecessors.head.successors.equals(::(leader, Nil)) =>
      true
  }

}
