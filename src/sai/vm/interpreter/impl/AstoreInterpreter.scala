package vm.interpreter.impl

import org.apache.bcel.generic.ASTORE
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object AstoreInterpreter extends InstructionInterpreter[ASTORE] {
  override def apply(i: ASTORE): Frame => Frame = {
    case frame @ Frame(_, _, stack, localVars, _) =>
      val slot = stack.peek
      val updatedStack = stack.pop
      val updatedLocalVars = localVars.set(i.getIndex, slot)
      frame.copy(stack = updatedStack, localVars = updatedLocalVars)
  }
}
