package vm.interpreter.impl

import org.apache.bcel.generic.GETFIELD
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object GetFieldInterpreter extends InstructionInterpreter[org.apache.bcel.generic.GETFIELD]{
  override def apply(i: GETFIELD): Frame => Frame = {
    frame =>
      frame
  }
}
