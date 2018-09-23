package vm.interpreter.impl

import sai.vm.DontCare
import vm.Frame
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object TrivialTransferFunction
    extends InterpreterBuilder[org.apache.bcel.generic.Instruction] {

  override def apply(i: org.apache.bcel.generic.Instruction): InstructionInterpreter = {
    case frame @ Frame(_, cpg, stack, _, _, _) =>
      var updatedStack = stack.pop(i.consumeStack(cpg))
      updatedStack = updatedStack.push(DontCare, i.produceStack(cpg))
      frame.copy(stack = updatedStack)
  }
}
