package vm.interpreter.impl

import org.apache.bcel.generic.INVOKESPECIAL
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object InvokeSpecialInterpreter extends InstructionInterpreter[INVOKESPECIAL] {
  override def apply(i: INVOKESPECIAL): Frame => Frame = {
    case frame @ Frame(_, cpg, stack, _, _) =>
      val updatedStack = stack.pop
      frame.copy(stack = updatedStack)
  }
}
