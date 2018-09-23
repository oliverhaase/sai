package vm.interpreter.impl

import org.apache.bcel.generic.ALOAD
import vm.Frame
import vm.interpreter.InterpreterBuilder
import vm.interpreter.InstructionInterpreter

private[interpreter] object AloadInterpreter extends InterpreterBuilder[ALOAD] {

  override def apply(i: ALOAD): InstructionInterpreter = {
    case frame @ Frame(_, _, stack, localVars, _, _) =>
      val slot         = localVars.get(i.getIndex)
      val updatedStack = stack.push(slot)
      frame.copy(stack = updatedStack)
  }
}
