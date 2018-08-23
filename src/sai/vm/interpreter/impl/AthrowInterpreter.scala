package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{ATHROW, ReferenceType, Type}
import sai.vm.ObjectRef.Null
import sai.vm.{OpStack, ObjectRef}
import vm.Frame
import vm.interpreter.InstructionInterpreter.Interpreter
import vm.interpreter.{Id, InstructionInterpreter}

private[interpreter] object AthrowInterpreter extends InstructionInterpreter[ATHROW] {

  override def apply(i: ATHROW): Interpreter = {
    case frame@Frame(m, _, stack, _, cg) =>
      val slot = stack.peek
      slot match {
        case Null =>
          // If objectref is null, athrow throws a NullPointerException instead of objectref.
          // see JVMS-8 p. 378
          val objectNode = ObjectNode(Id(m, i))
          val referenceNode = LocalReferenceNode(Id(m, i))
          val updatedCG =
            cg.addNodes(referenceNode, objectNode)
              .addEdge(referenceNode -> objectNode)
              .updateEscapeState(referenceNode -> NoEscape)
              .updateEscapeState(objectNode -> GlobalEscape)
          val referenceType = Type.getType(classOf[NullPointerException]).asInstanceOf[ReferenceType]
          val reference = ObjectRef(referenceType, referenceNode)
          val updatedStack = OpStack(reference :: Nil)
          frame.copy(cg = updatedCG, stack = updatedStack)
        case reference@ObjectRef(_, node) =>
          val objects = cg.pointsTo(node)
          val updatedCG = cg.updateEscapeStates(objects -> GlobalEscape)
          val updatedStack = OpStack(reference :: Nil)
          frame.copy(cg = updatedCG, stack = updatedStack)
      }
  }
}
