package vm.interpreter.impl

import cg.{NoEscape, _}
import org.apache.bcel.generic.{PUTFIELD, ReferenceType}
import sai.vm.ObjectRef.Null
import sai.vm.{OpStack, ObjectRef, Slot}
import vm.Frame
import vm.interpreter.InstructionInterpreter.Interpreter
import vm.interpreter.{Id, InstructionInterpreter}

private[interpreter] object PutFieldInterpreter extends InstructionInterpreter[PUTFIELD] {

  override def apply(i: PUTFIELD): Interpreter = {
    case frame@Frame(_, cpg, stack, _, _) =>
      val value :: reference :: rest = stack.elements
      val updatedStack = OpStack(rest)

      i.getFieldType(cpg) match {
        case _: ReferenceType =>
          (reference, value) match {
            case (_, Null) =>
              frame.copy(stack = updatedStack)
            case (Null, _) =>
              frame.copy(stack = updatedStack)
            case (_@ObjectRef(_, p), _@ObjectRef(_, q)) =>
              // we have an assignment in form of p.f = q
              val updatedCG = assignValueToReference(frame, i, p, q)
              frame.copy(stack = updatedStack, cg = updatedCG)
          }
        case _ =>
          frame.copy(stack = updatedStack)
      }
  }

  private def assignValueToReference(frame: Frame, i: PUTFIELD, p: ReferenceNode, q: ReferenceNode): ConnectionGraph = {

    var updatedCG = frame.cg

    val objectNodes = updatedCG.pointsTo(p) match {
      case nodes if nodes.isEmpty =>
        val phantomObjectNode = new PhantomObjectNode(Id(frame.method, i))
        updatedCG =
          updatedCG
            .addNode(phantomObjectNode)
            .addEdge(p -> phantomObjectNode)
            .updateEscapeState(phantomObjectNode -> ArgEscape)
        Set(phantomObjectNode)
      case nodes =>
        nodes
    }

    val fieldEdges = for {
      objectNode <- objectNodes
      fieldNode = FieldReferenceNode(objectNode, i.getFieldName(frame.cpg))
    } yield FieldEdge(objectNode -> fieldNode)

    val fieldNodes = for {
      fieldEdge <- fieldEdges
    } yield fieldEdge.to

    val deferredEdges = for {
      fieldNode <- fieldNodes
    } yield DeferredEdge(fieldNode -> q)

    updatedCG =
      updatedCG
        .addNodes(fieldNodes)
        .addEdges(fieldEdges ++ deferredEdges)
        .updateEscapeStates(fieldNodes -> NoEscape)
    updatedCG
  }

}
