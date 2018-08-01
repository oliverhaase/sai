package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.GETFIELD
import org.apache.bcel.generic.ReferenceType
import sai.vm.{DontCare, Null, Reference}
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.Id

private[interpreter] object GetFieldInterpreter extends InstructionInterpreter[GETFIELD] {
  override def apply(i: GETFIELD): Frame => Frame = {
    case frame@Frame(method, cpg, stack, _, cg) =>
      i.getFieldType(cpg) match {
        case refType: ReferenceType =>
          val q = stack.peek
          q match {
            case _@Null =>
              // see JVMS-8, p. 440
              throw new NullPointerException(s"null value is dereferenced in method '${method.name}' line ${method.lookup(i).lineNumber}!")
            case _@Reference(_, q: ReferenceNode) =>

              var updatedCG = cg

              val objectNodes = cg.pointsTo(q) match {
                case nodes if nodes.isEmpty =>
                  val phantomObjectNode = new PhantomObjectNode(Id(method, i))
                  updatedCG = updatedCG.addNode(phantomObjectNode)
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

              updatedCG = updatedCG
                .addNodes(fieldNodes ++ Set(referenceNode))
                .addEdges(fieldEdges ++ deferredEdges)

              val updatedStack = stack.push(Reference(refType, referenceNode))
              frame.copy(stack = updatedStack, cg = updatedCG)
          }
        case _ =>
          val updatedStack = stack.push(DontCare, i.produceStack(cpg))
          frame.copy(stack = updatedStack)
      }
  }
}
