package vm.interpreter.impl

import cg.{ArgEscape, PhantomReturnNode}
import org.apache.bcel.generic.{ARETURN, ObjectType}
import sai.vm.{ObjectRef, OpStack}
import sai.vm.ObjectRef.Null
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object AreturnInterpreter extends InstructionInterpreter[ARETURN] {
  override def apply(i: ARETURN): Interpreter = {
    case frame@Frame(method, _, stack, _, cg) =>
      assert(i.getType.isInstanceOf[ObjectType])
      stack.peek match {
        case Null =>
          val returnNode = new PhantomReturnNode(method.id)
          val updatedCG =
            cg.addNodes(returnNode)
              .updateEscapeState(returnNode -> ArgEscape)
          frame.copy(stack = OpStack(), cg = updatedCG)
        case _@ObjectRef(_, node) =>
          val returnNode = new PhantomReturnNode(method.id)
          val updatedCG =
            cg.addNodes(returnNode)
              .addEdge(returnNode -> node)
              .updateEscapeState(returnNode -> ArgEscape)
          frame.copy(stack = OpStack(), cg = updatedCG)
      }
  }
}
