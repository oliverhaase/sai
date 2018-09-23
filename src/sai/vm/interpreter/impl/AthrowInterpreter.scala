package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{ATHROW, ReferenceType, Type}
import sai.vm.{Null, Reference, OpStack}
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.{Id, InterpreterBuilder}

private[interpreter] object AthrowInterpreter extends InterpreterBuilder[ATHROW] {

  override def apply(i: ATHROW): InstructionInterpreter = {
    case frame @ Frame(m, _, stack, _, cg, _) =>
      val objectref = stack.peek

      (objectref: @unchecked) match {

        case reference @ Reference(_, node) =>
          val objects = node match {
            case o: ObjectNode    => Set(o)
            case r: ReferenceNode => cg.pointsTo(r)
          }
          val updatedCG    = cg.updateEscapeStates(objects -> GlobalEscape)
          val updatedStack = OpStack(reference :: Nil)
          frame.copy(stack = updatedStack, cg = updatedCG)

        case Null =>
          // If objectref is null, athrow throws a NullPointerException instead of objectref.
          // see JVMS-8 p. 378
          val objectNode = ObjectNode(Id(m, i))
          val updatedCG =
            cg.addNodes(objectNode)
              .updateEscapeState(objectNode -> GlobalEscape)
          val referenceType =
            Type.getType(classOf[NullPointerException]).asInstanceOf[ReferenceType]
          val reference    = Reference(referenceType, objectNode)
          val updatedStack = OpStack(reference :: Nil)
          frame.copy(cg = updatedCG, stack = updatedStack)
      }
  }
}
