package vm.interpreter.impl

import org.apache.bcel.generic.ALOAD
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object AloadInterpreter extends InstructionInterpreter[ALOAD] {

  override def apply(i: ALOAD): Interpreter = {
    case frame@Frame(_, _, stack, localVars, _) =>
      val slot = localVars.get(i.getIndex)
      val updatedStack = stack.push(slot)
      frame.copy(stack = updatedStack)
  }
}
