package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{AALOAD, Type}
import sai.vm.{Null, OpStack, Reference}
import vm.Frame
import vm.interpreter.{Helper, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object AaloadInterpreter extends InterpreterBuilder[AALOAD] {

  override def apply(i: AALOAD): InstructionInterpreter = new InstructionInterpreter {

    override def interpret(frame: Frame): List[Frame] = {
      val Frame(_, _, stack, _, cg) = frame

      val (arrayref, updatedStack) = (stack: @unchecked) match {
        case OpStack(_ :: array :: rest) => (array, OpStack(rest))
      }

      (arrayref: @unchecked) match {

        case Null =>
          frame.copy(stack = updatedStack.push(Null)) :: Nil

        case _ @Reference(referenceType, objectNode: ObjectNode) =>
          aaload(frame, cg, Set(objectNode), referenceType, updatedStack)

        case _ @Reference(referenceType, referenceNode: ReferenceNode) =>
          val (objectNodes, updatedCG) =
            Helper.getPointsToOrCreatePhantomObject(cg, referenceNode)
          aaload(frame, updatedCG, objectNodes, referenceType, updatedStack)
      }
    }

    private def aaload(frame: Frame,
                       cg: ConnectionGraph,
                       objects: Set[ObjectNode],
                       referenceType: Type,
                       stack: OpStack): List[Frame] = {
      for {
        obj                    <- objects.toList
        (fieldNode, updatedCG) = Helper.getOrCreateFieldNode(cg, obj, Helper.arrayFieldName)
        reference              = Reference(referenceType, fieldNode)
      } yield frame.copy(stack = stack.push(reference), cg = updatedCG)
    }

    override protected[interpreter] def doInterpret(frame: Frame): Frame =
      throw new UnsupportedOperationException
  }
}
