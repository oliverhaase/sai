package vm.interpreter.impl

import org.apache.bcel.generic.CHECKCAST
import sai.vm.Reference
import vm.Frame
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object CheckcastInterpreter extends InterpreterBuilder[CHECKCAST] {

  override def apply(i: CHECKCAST): InstructionInterpreter = {
    case frame: Frame =>
      val updatedStack = frame.stack.peek match {
        case r: Reference =>
          // we perform any cast, i.e., we do not check if the cast is legal
          frame.stack.pop.push(Reference(i.getLoadClassType(frame.cpg), r.node))
        case _ =>
          frame.stack
      }
      frame.copy(stack = updatedStack)
  }
}
