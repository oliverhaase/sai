package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{PUTFIELD, ReferenceType}
import sai.vm.{Null, Reference}
import vm.Frame
import vm.interpreter.{Id, InstructionInterpreter}

private[interpreter] object PutFieldInterpreter extends InstructionInterpreter[PUTFIELD] {
  override def apply(i: PUTFIELD): Frame => Frame = {
    case frame@Frame(method, cpg, stack, _, cg) =>
      var updatedStack = stack
      val qReference = updatedStack.peek
      updatedStack = updatedStack.pop
      val pReference = updatedStack.peek
      updatedStack = updatedStack.pop
      i.getFieldType(cpg) match {
        case _: ReferenceType =>
          (pReference, qReference) match {
            case (_@Null, _) =>
              // see JVMS-8 p. 551
              throw new NullPointerException(s"null value is dereferenced in method '${method.name}' line ${method.lookup(i).lineNumber}!")
            case (_, _@Null) =>
              // assigning a null value has no influence on the CG
              frame.copy(stack = updatedStack)
            case (_@Reference(_, p: ReferenceNode), _@Reference(_, q: ReferenceNode)) =>
              var updatedCG = cg

              val objectNodes = cg.pointsTo(p) match {
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
              val deferredEdges = for {fieldNode <- fieldNodes} yield DeferredEdge(fieldNode -> q)

              updatedCG = updatedCG
                .addNodes(fieldNodes)
                .addEdges(fieldEdges ++ deferredEdges)

              frame.copy(stack = updatedStack, cg = updatedCG)
          }
        case _ =>
          frame.copy(stack = updatedStack)
      }
  }
}
