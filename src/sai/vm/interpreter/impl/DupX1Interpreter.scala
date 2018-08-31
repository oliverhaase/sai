package vm.interpreter.impl

import org.apache.bcel.generic.DUP_X1
import sai.vm.OpStack
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object DupX1Interpreter extends InstructionInterpreter[DUP_X1] {
  override def apply(i: DUP_X1): Interpreter = {
    case frame@Frame(_, _, stack, _, _) =>
      val updatedStack = stack match {
        case OpStack(v1 :: v2 :: rest) => OpStack(v1 :: v2 :: v1 :: rest)
      }
      frame.copy(stack = updatedStack)
  }
}
