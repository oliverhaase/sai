package vm.interpreter.impl

import cg._
import org.apache.bcel.generic.NEW
import sai.vm.Reference
import vm.Frame
import vm.interpreter.{Helper, Id, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object NewInterpreter extends InterpreterBuilder[NEW] {

  override def apply(i: NEW): InstructionInterpreter = {
    case frame@Frame(method, cpg, stack, _, cg, _) =>
      val objectNode = ObjectNode(Id(method, i))
      val referenceType = i.getLoadClassType(cpg)
      val escapeState = Helper.determineEscapeState(referenceType)
      val updatedCG =
        cg.addNode(objectNode)
          .updateEscapeState(objectNode -> escapeState)
      val objectRef = Reference(referenceType, objectNode)
      val updatedStack = stack.push(objectRef)
      frame.copy(stack = updatedStack, cg = updatedCG)
  }

}
