package bytecode

import org.apache.bcel.generic.InvokeInstruction
import sai.bytecode.instruction.Instruction
import sai.bytecode.{Clazz, Method}

import scala.annotation.tailrec

class CallGraph(graph: Map[Method, List[Method]]) {

  def getSuccessors(method: Method): List[Method] = {
    assert(graph.keySet.contains(method))
    graph(method)
  }

  def isRecursive(method: Method): Boolean = {
    assert(graph.keySet.contains(method))
    getSuccessorsRecursive(method).contains(method)
  }

  def getSuccessorsRecursive(method: Method): List[Method] = {
    @tailrec
    def go(xs: List[Method], result: List[Method]): List[Method] = {
      xs match {
        case Nil =>
          result
        case h :: t if result.contains(h) =>
          go(t, result)
        case h :: t =>
          val unseen = getSuccessors(h).filterNot(result.contains)
          go(t ::: unseen, h :: result)
      }
    }
    go(getSuccessors(method), Nil)
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
    new CallGraph(callGraph.toMap)
  }

  def getSubProcedures(method: Method): List[Method] = {
    val cpg = method.cpg
    for {
      instruction       <- method.instructions if isInvokeInstruction(instruction)
      invokeInstruction = instruction.bcelInstruction.getInstruction.asInstanceOf[InvokeInstruction]
      className         = invokeInstruction.getClassName(cpg)
      methodName        = invokeInstruction.getMethodName(cpg)
      clazz             = new Clazz(className)
      method            <- clazz.method(methodName)
    } yield method
  }

  private def isInvokeInstruction(i: Instruction) = {
    Option(i.bcelInstruction).fold(false)(_.getInstruction.isInstanceOf[InvokeInstruction])
  }

}
