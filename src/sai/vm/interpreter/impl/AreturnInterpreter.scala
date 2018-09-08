package vm.interpreter.impl

import cg.{ArgEscape, PhantomReturnNode}
import org.apache.bcel.generic.ARETURN
import sai.vm.{Null, OpStack, Reference}
import vm.Frame
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object AreturnInterpreter extends InterpreterBuilder[ARETURN] {

  override def apply(i: ARETURN): InstructionInterpreter = {
    case frame @ Frame(method, _, stack, _, cg) =>
      val returnNode = new PhantomReturnNode(method.id)
      var updatedCG =
        cg.addNode(returnNode)
          .updateEscapeState(returnNode -> ArgEscape)
      updatedCG = (stack.peek: @unchecked) match {
        case Null =>
          cg
        case _ @Reference(_, node) =>
          updatedCG.addEdge(returnNode -> node)
      }
      frame.copy(stack = OpStack(), cg = updatedCG)
  }
}
