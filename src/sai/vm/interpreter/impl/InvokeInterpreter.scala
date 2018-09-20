package vm.interpreter.impl

import cg.ConnectionGraph
import org.apache.bcel.generic.InvokeInstruction
import sai.bytecode.{Clazz, Method}
import sai.vm.DontCare
import vm.Frame
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object InvokeInterpreter extends InterpreterBuilder[InvokeInstruction] {

  val cache = scala.collection.mutable.Map.empty[Method, ConnectionGraph]

  override def apply(i: InvokeInstruction): InstructionInterpreter = new InstructionInterpreter {
    override protected[interpreter] def doInterpret(frame: Frame): Frame = {
      val Frame(m, cpg, stack, _, _) = frame
      val updatedStack               = stack.pop(i.consumeStack(cpg)).push(DontCare, i.produceStack(cpg))
      val updatedCG                  = calc(i, frame)
      frame.copy(stack = updatedStack, cg = updatedCG)
    }
  }

  private def calc(i: InvokeInstruction, frame: Frame): ConnectionGraph = {
    val Frame(currentMethod, cpg, _, _, _) = frame

    val methodToInvoke = new Clazz(i.getClassName(cpg)).method(i.getMethodName(cpg)).get
    val recursive      = methodToInvoke.callGraph.isRecursive(methodToInvoke)

    if (!recursive) {
      frame.cg.merge(methodToInvoke.summary)
    } else {
      val cached = cache.contains(currentMethod)
      val before = cache.getOrElse(currentMethod, currentMethod.nonRecursiveSummary)
      cache(currentMethod) =
        before.merge(cache.getOrElse(methodToInvoke, methodToInvoke.nonRecursiveSummary))
      val after = cache(currentMethod)
      if (cached && before == after) {
        frame.cg.merge(cache(methodToInvoke))
      } else {
        frame.cg.merge(methodToInvoke.summary)
      }
    }
  }

}
