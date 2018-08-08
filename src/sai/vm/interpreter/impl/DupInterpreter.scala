package vm.interpreter.impl

import org.apache.bcel.generic.DUP
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object DupInterpreter extends InstructionInterpreter[DUP] {

  override def apply(i: DUP): Interpreter = {
    case frame@Frame(_, _, stack, _, _) =>
      val updatedStack = stack.dup
      frame.copy(stack = updatedStack)
  }
}
