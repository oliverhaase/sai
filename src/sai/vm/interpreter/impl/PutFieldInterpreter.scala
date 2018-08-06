package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{PUTFIELD, ReferenceType}
import sai.vm.{OpStack, Reference, Slot}
import sai.vm.Reference.Null
import vm.Frame
import vm.interpreter.{Id, InstructionInterpreter}
import cg.NoEscape

private[interpreter] object PutFieldInterpreter extends InstructionInterpreter[PUTFIELD] {
  override def apply(i: PUTFIELD): Frame => Frame = {
    case frame@Frame(_, cpg, stack, _, _) =>

      val (valueSlot, referenceSlot, updatedStack) = pop2(stack)

      val frames = for {
        value <- valueSlot
        reference <- referenceSlot
      } yield {
        i.getFieldType(cpg) match {
          case _: ReferenceType =>
            (value, reference) match {
              case (_, Null) =>
                frame.copy(stack = updatedStack)
              case (Null, _) =>
                // assigning a null value has no influence on the CG
                frame.copy(stack = updatedStack)
              case (_@Reference(_, q: ReferenceNode), _@Reference(_, p: ReferenceNode)) =>
                // we have an assignment in form of p.f = q
                val updatedCG = assignValueToReference(frame, i, p, q)
                frame.copy(stack = updatedStack, cg = updatedCG)
            }
          case _ =>
            frame.copy(stack = updatedStack)
        }
      }

      val resultFrame = frames.reduce(_ merge _)
      resultFrame
  }

  private def pop2(stack: OpStack): (Slot, Slot, OpStack) = {
    val value = stack.peek
    var updatedStack = stack.pop
    val objectRef = updatedStack.peek
    updatedStack = updatedStack.pop
    (value, objectRef, updatedStack)
  }

  private def assignValueToReference(frame: Frame, i: PUTFIELD, p: ReferenceNode, q: ReferenceNode): ConnectionGraph = {

    var updatedCG = frame.cg

    val objectNodes = updatedCG.pointsTo(p) match {
      case nodes if nodes.isEmpty =>
        val phantomObjectNode = new PhantomObjectNode(Id(frame.method, i))
        updatedCG = updatedCG.addNode(phantomObjectNode).updateEscapeState(phantomObjectNode -> ArgEscape)
        Set(phantomObjectNode)
      case nodes =>
        nodes
    }

    val fieldEdges = for {
      objectNode <- objectNodes
      fieldNode = FieldReferenceNode(objectNode, i.getFieldName(frame.cpg))
    } yield FieldEdge(objectNode -> fieldNode)

    val fieldNodes = for {fieldEdge <- fieldEdges} yield fieldEdge.to
    val deferredEdges = for {fieldNode <- fieldNodes} yield DeferredEdge(fieldNode -> q)
    val allEdges = fieldEdges ++ deferredEdges

    updatedCG = updatedCG
      .addNodes(fieldNodes)
      .addEdges(allEdges)
      .updateEscapeStates(fieldNodes -> NoEscape)
    updatedCG
  }

}
