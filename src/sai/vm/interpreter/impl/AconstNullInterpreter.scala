package vm.interpreter.impl

import org.apache.bcel.generic.ACONST_NULL
import sai.vm.Null
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object AconstNullInterpreter extends InstructionInterpreter[ACONST_NULL] {
  override def apply(i: ACONST_NULL): Frame => Frame = {
    case frame @ Frame(_, _, stack, _, _) =>
      val updatedStack = stack.push(Null)
      frame.copy(stack = updatedStack)
  }
}
