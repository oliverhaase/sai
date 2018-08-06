package vm.interpreter.impl

import org.apache.bcel.generic.SWAP
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object SwapInterpreter extends InstructionInterpreter[SWAP] {

  override def apply(i: SWAP): Frame => Frame = {
    case frame@Frame(_, _, stack, _, _) =>
      val updatedStack = stack.swap
      frame.copy(stack = updatedStack)
  }
}
