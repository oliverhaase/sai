package vm

import bytecode.{BasicBlock, GraphInfo}
import cg.ConnectionGraph
import org.apache.bcel.generic.InvokeInstruction
import sai.bytecode.{Method, Program}

object NonRecursiveSummary {

  def apply(method: Method): ConnectionGraph = {

    // a block contains a recursive call if it contains an invoke instruction which
    // would cause 'method' to be invoked again (recursion)
    def containsRecursiveCall(block: BasicBlock): Boolean = {
      block.instructions.exists { i =>
        Option(i.bcelInstruction).map(_.getInstruction) match {
          case Some(invokeInstruction: InvokeInstruction) =>
            val clazz = Program.getClass(invokeInstruction.getClassName(method.cpg))
            clazz.method(invokeInstruction.getMethodName(method.cpg)) match {
              case Some(m) if m.callGraph.recursive().contains(method) =>
                true
              case _ =>
                false
            }
          case _ => false
        }
      }
    }

    val nonRecursiveBlocks = GraphInfo.findPassableNodes[BasicBlock](
      allNodes = method.controlFlowGraph,
      impassableNodes = method.controlFlowGraph.filter(containsRecursiveCall),
      findSuccessors = block => block.successors,
      findPredecessors = block => block.predecessors
    )

    if (nonRecursiveBlocks.isEmpty) {
      ConnectionGraph.empty()
    } else {
      IntraproceduralAnalysis(
        Frame(method),
        nonRecursiveBlocks,
        findSuccessors = block => block.successors.filter(nonRecursiveBlocks.contains),
        findPredecessors = block => block.predecessors.filter(nonRecursiveBlocks.contains)
      )
    }
  }

}
