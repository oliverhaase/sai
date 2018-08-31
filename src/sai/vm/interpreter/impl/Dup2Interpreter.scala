package vm.interpreter.impl

import org.apache.bcel.generic.DUP2
import sai.vm.OpStack
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object Dup2Interpreter extends InstructionInterpreter[DUP2] {
  override def apply(i: DUP2): Interpreter = {
    case frame@Frame(_, _, stack, _, _) =>
      val updatedStack = stack match {
        case OpStack(v1 :: v2 :: rest) => OpStack(v1 :: v2 :: v1 :: v2 :: rest)
      }
      frame.copy(stack = updatedStack)
  }
}
