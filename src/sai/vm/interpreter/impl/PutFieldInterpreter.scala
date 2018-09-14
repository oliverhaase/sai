package vm.interpreter.impl

import cg.{NoEscape, _}
import org.apache.bcel.generic.{PUTFIELD, ReferenceType}
import sai.vm.{OpStack, Reference}
import vm.Frame
import vm.interpreter.{Helper, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object PutFieldInterpreter extends InterpreterBuilder[PUTFIELD] {

  override def apply(i: PUTFIELD): InstructionInterpreter = {
    case frame @ Frame(_, cpg, stack, _, cg) =>
      val value :: reference :: rest = stack.elements
      val updatedStack               = OpStack(rest)

      val updatedCG = i.getFieldType(cpg) match {
        case _: ReferenceType =>
          val f = i.getFieldName(cpg)
          (reference, value) match {
            case (_ @Reference(_, p: ObjectNode), _ @Reference(_, q)) =>
              assign(cg, p, f, q)
            case (_ @Reference(_, p: ReferenceNode), _ @Reference(_, q)) =>
              val (objects, updatedCG) =
                Helper.getPointsToOrCreatePhantomObject(cg, p)
              objects.foldLeft(updatedCG)((cg, p) => assign(cg, p, f, q))
            case _ =>
              cg
          }
        case _ =>
          cg
      }
      frame.copy(stack = updatedStack, cg = updatedCG)
  }

  private def assign(cg: ConnectionGraph, p: ObjectNode, fieldname: String, q: Node) = {
    val f = FieldReferenceNode(p, fieldname)
    cg.addNode(f)
      .updateEscapeState(f -> NoEscape)
      .addEdge(p -> f)
      .addEdge(f -> q)
  }

}
