package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.AASTORE
import sai.vm.{Null, OpStack, Reference, Slot}
import vm.Frame
import vm.interpreter.{Helper, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object AastoreInterpreter extends InterpreterBuilder[AASTORE] {

  override def apply(i: AASTORE): InstructionInterpreter =
    new InstructionInterpreter {
      override def interpret(frame: Frame): List[Frame] = {
        val Frame(_, _, stack, _, cg) = frame
        val value :: _ :: arrayRef :: rest = stack.elements
        val updatedStack                   = OpStack(rest)

        (arrayRef: @unchecked) match {

          case Null =>
            frame.copy(stack = updatedStack) :: Nil

          case _ @Reference(_, objectNode: ObjectNode) =>
            aastore(frame, cg, Set(objectNode), updatedStack, value)

          case _ @Reference(_, referenceNode: ReferenceNode) =>
            val (objectNodes, updatedCG) =
              Helper.getPointsToOrCreatePhantomObject(cg, referenceNode)
            aastore(frame, updatedCG, objectNodes, updatedStack, value)
        }
      }

      private def aastore(frame: Frame,
                          cg: ConnectionGraph,
                          objects: Set[ObjectNode],
                          updatedStack: OpStack,
                          value: Slot): List[Frame] = {
        for {
          obj                <- objects.toList
          (fieldNode, newCG) = Helper.getOrCreateFieldNode(cg, obj, Helper.arrayFieldName)
          updatedCG = (value: @unchecked) match {
            case Null =>
              newCG
            case Reference(_, valueNode) =>
              newCG.addEdge(fieldNode -> valueNode)
          }
        } yield frame.copy(stack = updatedStack, cg = updatedCG)
      }

      override protected[interpreter] def doInterpret(frame: Frame): Frame =
        throw new UnsupportedOperationException
    }
}
