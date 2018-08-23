package vm.interpreter.impl

import org.apache.bcel.generic.ACONST_NULL
import sai.vm.ObjectRef.Null
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object AconstNullInterpreter extends InstructionInterpreter[ACONST_NULL] {

  override def apply(i: ACONST_NULL): Interpreter = {
    case frame@Frame(_, _, stack, _, _) =>
      val updatedStack = stack.push(Null)
      frame.copy(stack = updatedStack)
  }
}
