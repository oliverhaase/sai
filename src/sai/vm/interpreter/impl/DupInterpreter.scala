package vm.interpreter.impl

import org.apache.bcel.generic.DUP
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object DupInterpreter extends InstructionInterpreter[DUP] {
  override def apply(i: DUP): Frame => Frame = {
    case frame @ Frame(_, _, stack, _, _) =>
      val updatedStack = stack.dup
      frame.copy(stack = updatedStack)
  }
}
