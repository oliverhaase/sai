package vm.interpreter.impl

import org.apache.bcel.generic.ACONST_NULL
import sai.vm.Null
import vm.Frame
import vm.interpreter.InterpreterBuilder
import vm.interpreter.InstructionInterpreter

private[interpreter] object AconstNullInterpreter extends InterpreterBuilder[ACONST_NULL] {

  override def apply(i: ACONST_NULL): InstructionInterpreter = {
    case frame @ Frame(_, _, stack, _, _, _) =>
      val updatedStack = stack.push(Null)
      frame.copy(stack = updatedStack)
  }
}
