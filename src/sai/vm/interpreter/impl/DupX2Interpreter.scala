package vm.interpreter.impl

import org.apache.bcel.generic.DUP_X2
import sai.vm.OpStack
import vm.Frame
import vm.interpreter.InterpreterBuilder
import vm.interpreter.InstructionInterpreter

private[interpreter] object DupX2Interpreter extends InterpreterBuilder[DUP_X2] {

  override def apply(i: DUP_X2): InstructionInterpreter = {
    case frame @ Frame(_, _, stack, _, _) =>
      val updatedStack = (stack: @unchecked) match {
        case OpStack(v1 :: v2 :: v3 :: rest) =>
          OpStack(v1 :: v2 :: v3 :: v1 :: rest)
      }
      frame.copy(stack = updatedStack)
  }
}
