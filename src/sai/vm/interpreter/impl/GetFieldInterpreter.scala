package vm.interpreter.impl

import ea._
import org.apache.bcel.generic.{GETFIELD, ReferenceType, Type}
import sai.vm._
import vm.Frame
import vm.interpreter.{Helper, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object GetFieldInterpreter extends InterpreterBuilder[GETFIELD] {

  override def apply(i: GETFIELD): InstructionInterpreter = new InstructionInterpreter {
    override def interpret(frame: Frame): List[Frame] = {
      val Frame(_, cpg, stack, _, cg, _) = frame
      val objectref :: rest                = stack.elements
      val updatedStack                     = OpStack(rest)

      i.getFieldType(cpg) match {
        case _: ReferenceType =>
          val fieldname = i.getFieldName(cpg)
          (objectref: @unchecked) match {

            case Null =>
              frame.copy(stack = updatedStack.push(Null)) :: Nil

            case _ @Reference(referenceType, objectNode: ObjectNode) =>
              getfields(frame, cg, updatedStack, i.getFieldType(cpg), fieldname, Set(objectNode))

            case _ @Reference(referenceType, referenceNode: ReferenceNode) =>
              val (objects, newCG) =
                Helper.getPointsToOrCreatePhantomObject(cg, referenceNode)
              getfields(frame, newCG, updatedStack, i.getFieldType(cpg), fieldname, objects)
          }
        case _ =>
          frame.copy(stack = updatedStack.push(DontCare, i.produceStack(cpg))) :: Nil
      }
    }

    override protected[interpreter] def doInterpret(frame: Frame): Frame =
      throw new UnsupportedOperationException
  }

  private def getfields(frame: Frame,
                        cg: ConnectionGraph,
                        stack: OpStack,
                        referenceType: Type,
                        fieldname: String,
                        objects: Set[ObjectNode]): List[Frame] = {
    for {
      obj                <- objects.toList
      (field, updatedCG) = Helper.getOrCreateFieldNode(cg, obj, fieldname)
      reference          = Reference(referenceType, field)
    } yield frame.copy(stack = stack.push(reference), cg = updatedCG)
  }

}
