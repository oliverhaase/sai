package bytecode

import org.apache.bcel.generic.InvokeInstruction
import sai.bytecode.instruction.Instruction
import sai.bytecode.{Clazz, Method, Program}

import scala.annotation.tailrec

class CallGraph(graph: Map[Method, List[Method]], method: Method) {

  def getSuccessors(): List[Method] = {
    graph(method)
  }

  def isRecursive(): Boolean = {
    recursive().contains(method)
  }

  def recursive(): List[Method] = {
    @tailrec
    def go(xs: List[Method], result: List[Method]): List[Method] = {
      xs match {
        case Nil =>
          result
        case h :: t if result.contains(h) =>
          go(t, result)
        case h :: t =>
          val unseen = graph(h).filterNot(result.contains)
          go(t ::: unseen, h :: result)
      }
    }
    go(getSuccessors(), Nil)
  }

}

object CallGraph {

  def apply(method: Method): CallGraph = buildCallGraph(method)

  private def buildCallGraph(method: Method): CallGraph = {
    val callGraph = scala.collection.mutable.Map.empty[Method, List[Method]]

    def analyze(m: Method): Unit = {
      callGraph(m) = getSubProcedures(m).distinct
      for (s <- callGraph(m) if !callGraph.contains(s)) {
        analyze(s)
      }
    }

    analyze(method)
    new CallGraph(callGraph.toMap, method)
  }

  def getSubProcedures(method: Method): List[Method] = {
    val cpg = method.cpg
    for {
      instruction       <- method.instructions if isInvokeInstruction(instruction)
      invokeInstruction = instruction.bcelInstruction.getInstruction.asInstanceOf[InvokeInstruction]
      className         = invokeInstruction.getClassName(cpg)
      methodName        = invokeInstruction.getMethodName(cpg)
      clazz             = Program.getClass(className)
      method            <- clazz.method(methodName)
    } yield method
  }

  private def isInvokeInstruction(i: Instruction) = {
    Option(i.bcelInstruction).fold(false)(_.getInstruction.isInstanceOf[InvokeInstruction])
  }

}
