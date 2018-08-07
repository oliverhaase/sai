package vm.interpreter.impl

import cg.{NoEscape, _}
import org.apache.bcel.generic.{GETFIELD, ReferenceType}
import sai.vm.Reference.Null
import sai.vm.{DontCare, Reference}
import vm.Frame
import vm.interpreter.{Id, InstructionInterpreter}

private[interpreter] object GetFieldInterpreter extends InstructionInterpreter[GETFIELD] {
  override def apply(i: GETFIELD): Frame => Frame = {
    case frame@Frame(_, cpg, stack, _, _) =>

      val slot = stack.peek
      var updatedStack = stack.pop

      val frames = for {
        slot <- slot
      } yield {
        i.getFieldType(cpg) match {
          case refType: ReferenceType =>
            slot match {
              case Null =>
                updatedStack = updatedStack.push(Null)
                frame.copy(stack = updatedStack)
              case _@Reference(_, q: ReferenceNode) =>
                val (referenceNode, updatedCG) = getFieldReference(q, frame, i)
                updatedStack = updatedStack.push(Reference(refType, referenceNode))
                frame.copy(stack = updatedStack, cg = updatedCG)
            }
          case _ =>
            updatedStack = updatedStack.push(DontCare)
            frame.copy(stack = updatedStack)
        }
      }

      val resultFrame = frames.reduce(_ merge _)
      resultFrame
  }

  private def getFieldReference(q: ReferenceNode, frame: Frame, i: GETFIELD): (ReferenceNode, ConnectionGraph) = {
    var updatedCG = frame.cg

    val objectNodes = updatedCG.pointsTo(q) match {
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

    val referenceNode = LocalReferenceNode(Id(frame.method, i))
    val deferredEdges = for {
      fieldNode <- fieldNodes
    } yield DeferredEdge(referenceNode -> fieldNode)

    val allNodes = fieldNodes ++ Set(referenceNode)

    updatedCG = updatedCG
      .addNodes(allNodes)
      .addEdges(fieldEdges ++ deferredEdges)
      .updateEscapeStates(allNodes -> NoEscape)

    (referenceNode, updatedCG)
  }
}
