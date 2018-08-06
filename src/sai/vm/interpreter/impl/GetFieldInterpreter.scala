package vm.interpreter.impl

import cg.{NoEscape, _}
import org.apache.bcel.generic.{GETFIELD, ReferenceType}
import sai.vm.Reference.Null
import sai.vm.{DontCare, Reference}
import vm.Frame
import vm.interpreter.{Id, InstructionInterpreter}

private[interpreter] object GetFieldInterpreter extends InstructionInterpreter[GETFIELD] {
  override def apply(i: GETFIELD): Frame => Frame = {
    case frame@Frame(method, cpg, stack, _, cg) =>

      val slot = stack.peek

      val frames = for {
        slot <- slot
      } yield {
        i.getFieldType(cpg) match {
          case refType: ReferenceType =>
            slot match {
              case Null =>
                val updatedStack = stack.pop.push(Null)
                frame.copy(stack = updatedStack)
              case _@Reference(_, q: ReferenceNode) =>

                var updatedCG = cg

                val objectNodes = updatedCG.pointsTo(q) match {
                  case nodes if nodes.isEmpty =>
                    val phantomObjectNode = new PhantomObjectNode(Id(method, i))
                    updatedCG = updatedCG.addNode(phantomObjectNode).updateEscapeState(phantomObjectNode -> ArgEscape)
                    Set(phantomObjectNode)
                  case nodes =>
                    nodes
                }

                val fieldEdges = for {
                  objectNode <- objectNodes
                  fieldNode = FieldReferenceNode(objectNode, i.getFieldName(cpg))
                } yield FieldEdge(objectNode -> fieldNode)

                val fieldNodes = for {fieldEdge <- fieldEdges} yield fieldEdge.to
                val referenceNode = LocalReferenceNode(Id(method, i))
                val deferredEdges = for {fieldNode <- fieldNodes} yield DeferredEdge(referenceNode -> fieldNode)

                val allNodes = fieldNodes ++ Set(referenceNode)
                val allEdges = fieldEdges ++ deferredEdges

                updatedCG = updatedCG
                  .addNodes(allNodes)
                  .addEdges(allEdges)
                  .updateEscapeStates(allNodes -> NoEscape)

                val updatedStack = stack.pop.push(Reference(refType, referenceNode))
                frame.copy(stack = updatedStack, cg = updatedCG)
            }
          case _ =>
            val updatedStack = stack.pop.push(DontCare, i.produceStack(cpg))
            frame.copy(stack = updatedStack)
        }
      }

      val resultFrame = frames.reduce(_ merge _)
      resultFrame
  }

}
