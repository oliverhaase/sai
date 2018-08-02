package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.{NEW, ObjectType}
import sai.vm.Reference
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.Id

private[interpreter] object NewInterpreter extends InstructionInterpreter[NEW] {
  override def apply(i: NEW): Frame => Frame = {
    case frame@Frame(method, cpg, stack, _, cg) =>
      val id = Id(method, i)
      val objectNode = ObjectNode(id)
      val localReferenceNode = LocalReferenceNode(id)
      val referenceType = i.getLoadClassType(cpg)
      val escapeState = determineEscapeState(referenceType)
      val updatedCG =
          cg
          .addNodes(localReferenceNode, objectNode)
          .addEdge(localReferenceNode -> objectNode)
          .updateEscapeState(objectNode -> escapeState)
      val objectRef = Reference(referenceType, localReferenceNode)
      val updatedStack = stack.push(objectRef)
      frame.copy(stack = updatedStack, cg = updatedCG)
  }

  private def determineEscapeState(objectType: ObjectType): EscapeState = {
    Class.forName(objectType.getClassName).getInterfaces match {
      case interfaces if interfaces.contains(classOf[java.lang.Runnable]) => GlobalEscape
      case _ => NoEscape
    }
  }
}