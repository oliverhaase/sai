package vm.interpreter.impl

import sai.vm.DontCare
import vm.Frame
import vm.interpreter.InstructionInterpreter

private[interpreter] object TrivialTransferFunction extends InstructionInterpreter[org.apache.bcel.generic.Instruction] {

  override def apply(i: org.apache.bcel.generic.Instruction): Frame => Frame = {
    case frame@Frame(_, cpg, stack, _, _) =>
      var updatedStack = stack.pop(i.consumeStack(cpg))
      updatedStack = updatedStack.push(DontCare, i.produceStack(cpg))
      frame.copy(stack = updatedStack)
  }
}
