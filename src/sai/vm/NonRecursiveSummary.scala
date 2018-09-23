package vm

import bytecode.{BasicBlock, GraphInfo}
import cg.ConnectionGraph
import org.apache.bcel.generic.InvokeInstruction
import sai.bytecode.{Method, Program}

object NonRecursiveSummary {

  def apply(method: Method): ConnectionGraph = {

    val cpg = method.cpg

    def containsRecursiveCall(block: BasicBlock): Boolean = {
      block.instructions.exists { i =>
        Option(i.bcelInstruction).map(_.getInstruction) match {
          case Some(invokeInstruction: InvokeInstruction) =>
            val clazz = Program.getClass(invokeInstruction.getClassName(cpg))
            clazz.method(invokeInstruction.getMethodName(cpg)) match {
              case Some(m) if m.callGraph.recursive().contains(method) =>
                true
              case _ =>
                false
            }
          case _ => false
        }
      }
    }

    def findNonRecursiveSuccessorBlocks(block: BasicBlock): List[BasicBlock] = {
      block.successors.filterNot(containsRecursiveCall)
    }

    def findNonRecursivePredecessorBlocks(block: BasicBlock): List[BasicBlock] = {
      block.predecessors.filterNot(containsRecursiveCall)
    }

    val nonRecursivePathExists = GraphInfo.pathExists[BasicBlock](
      from = method.controlFlowGraph.head,
      to = method.controlFlowGraph.last,
      findSuccessors = findNonRecursiveSuccessorBlocks
    )

    if (!nonRecursivePathExists) {
      ConnectionGraph.empty()
    } else {

      val nonRecursiveBasicBlocks = for {
        basicBlock <- method.controlFlowGraph if !containsRecursiveCall(basicBlock)
      } yield basicBlock

      IntraproceduralAnalysis(Frame(method),
                              nonRecursiveBasicBlocks,
                              findNonRecursiveSuccessorBlocks,
                              findNonRecursivePredecessorBlocks)
    }
  }

}
