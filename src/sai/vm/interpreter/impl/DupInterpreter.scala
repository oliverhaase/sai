package vm.interpreter.impl

import org.apache.bcel.generic.DUP
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object DupInterpreter extends InstructionInterpreter[org.apache.bcel.generic.DUP] {
  override def apply(i: DUP): Frame => Frame = {
    frame =>
      val updatedStack = frame.opStack.dup
      val updatedFrame = frame.copy(opStack = updatedStack)
      updatedFrame
  }
}
