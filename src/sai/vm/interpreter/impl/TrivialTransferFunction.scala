package vm.interpreter.impl

import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object TrivialTransferFunction extends InstructionInterpreter[org.apache.bcel.generic.Instruction] {
  override def apply(i: org.apache.bcel.generic.Instruction): Frame => Frame = {
    frame =>
      frame
  }
}
