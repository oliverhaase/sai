package vm.interpreter.impl

import org.apache.bcel.generic.PUTFIELD
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object PutFieldInterpreter extends InstructionInterpreter[org.apache.bcel.generic.PUTFIELD] {
  override def apply(i: PUTFIELD): Frame => Frame = {
    frame =>
      frame
  }
}
