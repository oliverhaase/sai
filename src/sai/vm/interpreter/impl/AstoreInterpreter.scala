package vm.interpreter.impl

import cg.{LocalReferenceNode, NoEscape}
import org.apache.bcel.generic.ASTORE
import sai.vm.{DontCare, Null, Reference}
import vm.Frame
import vm.interpreter.InterpreterBuilder
import vm.interpreter.InstructionInterpreter

private[interpreter] object AstoreInterpreter extends InterpreterBuilder[ASTORE] {

  override def apply(i: ASTORE): InstructionInterpreter = {
    case frame @ Frame(m, _, stack, localVars, cg) =>
      val objectref          = stack.peek
      val localReferenceNode = LocalReferenceNode(i.getIndex.toString)

      val (updatedCG, newSlot) = objectref match {

        case Reference(referenceType, node) =>
          val updatedCG =
            if (m.lookup(i).isInTryRange) {
              // loss of precession since we also don't kill local vars created inside try range
              cg.addNode(localReferenceNode)
                .addEdge(localReferenceNode -> node)
                .updateEscapeState(localReferenceNode -> NoEscape)
            } else {
              cg.byPass(localReferenceNode)
                .addNode(localReferenceNode)
                .addEdge(localReferenceNode -> node)
                .updateEscapeState(localReferenceNode -> NoEscape)
            }
          (updatedCG, Reference(referenceType, localReferenceNode))

        case _ =>
          val updatedCG =
            if (m.lookup(i).isInTryRange)
              cg
            else
              cg.byPass(localReferenceNode)
          (updatedCG, objectref)
      }
      val updatedStack     = stack.pop
      val updatedLocalVars = localVars.set(i.getIndex, newSlot)
      frame.copy(stack = updatedStack, localVars = updatedLocalVars, cg = updatedCG)
  }
}
