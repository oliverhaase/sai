package vm.interpreter.impl

import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object AloadInterpreter extends InstructionInterpreter[org.apache.bcel.generic.ALOAD] {
  override def apply(i: org.apache.bcel.generic.ALOAD): Frame => Frame = {
    frame =>
      val objectRef = frame.localVars.get(i.getIndex)
      val updatedStack = frame.opStack.push(objectRef)
      val updatedFrame = frame.copy(opStack = updatedStack)
      updatedFrame
  }
}
