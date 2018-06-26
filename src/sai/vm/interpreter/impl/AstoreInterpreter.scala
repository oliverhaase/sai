package vm.interpreter.impl

import org.apache.bcel.generic.ASTORE
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object AstoreInterpreter extends InstructionInterpreter[org.apache.bcel.generic.ASTORE] {
  override def apply(i: ASTORE): Frame => Frame = {
    frame =>
      val (objectRef, updatedStack) = frame.opStack.pop
      val updatedLocalVars = frame.localVars.set(i.getIndex, objectRef)
      val updatedFrame = frame.copy(opStack = updatedStack, localVars = updatedLocalVars)
      updatedFrame
  }
}
