package vm.interpreter.impl

import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object InvokeSpecialInterpreter extends InstructionInterpreter[org.apache.bcel.generic.INVOKESPECIAL] {
  override def apply(i: org.apache.bcel.generic.INVOKESPECIAL): Frame => Frame = {
    frame =>
      val (_, updatedStack) = frame.opStack.pop
      val updatedFrame = frame.copy(opStack = updatedStack)
      updatedFrame
  }
}
