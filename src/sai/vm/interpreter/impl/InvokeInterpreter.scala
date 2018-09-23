package vm.interpreter.impl

import cg.ConnectionGraph
import org.apache.bcel.generic.InvokeInstruction
import sai.bytecode.{Clazz, Method, Program}
import sai.vm.{DontCare, OpStack}
import vm.{Frame, IntraproceduralAnalysis}
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object InvokeInterpreter extends InterpreterBuilder[InvokeInstruction] {

  override def apply(i: InvokeInstruction): InstructionInterpreter = new InstructionInterpreter {
    override protected[interpreter] def doInterpret(frame: Frame): Frame = {
      val Frame(_, cpg, stack, _, _, _) = frame

      val updatedCG    = frame.cg.merge(calc(i, frame))
      val updatedStack = stack.pop(i.consumeStack(cpg)).push(DontCare)
      frame.copy(stack = updatedStack, cg = updatedCG)
    }
  }

  def calc(i: InvokeInstruction, frame: Frame): ConnectionGraph = {
    val Frame(currentMethod, cpg, _, _, _, cache) = frame

    val methodToInvoke = Program.getClass(i.getClassName(cpg)).method(i.getMethodName(cpg)).get

    // get all successors recursive
    val recursiveSuccessors = methodToInvoke.callGraph.recursive()

    // check if call of 'methodToInvoke' causes 'currentMethod' to be called -> endless recursion!
    val isRecursive = recursiveSuccessors.contains(currentMethod)

    if (!isRecursive) {
      methodToInvoke.summary
    } else {
      val cgBefore         = cache.getOrElse(currentMethod, currentMethod.nonRecursiveSummary)
      val cgMethodToInvoke = cache.getOrElse(methodToInvoke, methodToInvoke.nonRecursiveSummary)
      val cgAfter          = cgBefore.merge(cgMethodToInvoke)

      if (cgBefore == cgAfter && cache.contains(methodToInvoke)) {
        cache(methodToInvoke)
      } else {
        val initialFrame =
          Frame(methodToInvoke).copy(cache = cache + (currentMethod -> cgAfter))
        IntraproceduralAnalysis(initialFrame,
                                methodToInvoke.controlFlowGraph,
                                _.successors,
                                _.predecessors)
      }
    }
  }

}
