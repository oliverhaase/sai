package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{PUTFIELD, ReferenceType}
import sai.vm.{OpStack, Reference, Slot}
import sai.vm.Reference.Null
import vm.Frame
import vm.interpreter.InstructionInterpreter.Interpreter
import vm.interpreter.{Id, InstructionInterpreter}
import cg.NoEscape

private[interpreter] object PutFieldInterpreter extends InstructionInterpreter[PUTFIELD] {

  override def apply(i: PUTFIELD): Interpreter = {
    case frame@Frame(_, cpg, stack, _, _) =>

      val (valueSlot, referenceSlot, updatedStack) = pop2(stack)

      val frames = for {
        reference <- referenceSlot
        value <- valueSlot
      } yield {
        i.getFieldType(cpg) match {
          case _: ReferenceType =>
            (reference, value) match {
              case (_, Null) =>
                frame.copy(stack = updatedStack)
              case (Null, _) =>
                frame.copy(stack = updatedStack)
              case (_@Reference(_, p), _@Reference(_, q)) =>
                // we have an assignment in form of p.f = q
                val updatedCG = assignValueToReference(frame, i, p, q)
                frame.copy(stack = updatedStack, cg = updatedCG)
            }
          case _ =>
            frame.copy(stack = updatedStack)
        }
      }

      val outFrame = frames.reduce(_ merge _)
      outFrame
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
        updatedCG =
          updatedCG
            .addNode(phantomObjectNode)
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
