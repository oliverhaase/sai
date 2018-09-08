package vm.interpreter.impl

import org.apache.bcel.generic.SWAP
import sai.vm.OpStack
import vm.Frame
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object SwapInterpreter extends InterpreterBuilder[SWAP] {

  override def apply(i: SWAP): InstructionInterpreter = {
    case frame @ Frame(_, _, stack, _, _) =>
      val updatedStack = (stack: @unchecked) match {
        case OpStack(v1 :: v2 :: rest) =>
          OpStack(v2 :: v1 :: rest)
      }
      frame.copy(stack = updatedStack)
  }
}
