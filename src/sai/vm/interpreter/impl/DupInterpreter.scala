package vm.interpreter.impl

import org.apache.bcel.generic.DUP
import sai.vm.OpStack
import vm.Frame
import vm.interpreter.InterpreterBuilder
import vm.interpreter.InstructionInterpreter

private[interpreter] object DupInterpreter extends InterpreterBuilder[DUP] {

  override def apply(i: DUP): InstructionInterpreter = {
    case frame @ Frame(_, _, stack, _, _) =>
      val updatedStack = (stack: @unchecked) match {
        case OpStack(v :: rest) =>
          OpStack(v :: v :: rest)
      }
      frame.copy(stack = updatedStack)
  }
}
