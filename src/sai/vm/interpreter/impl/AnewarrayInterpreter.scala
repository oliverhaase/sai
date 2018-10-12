package vm.interpreter.impl

import ea.{NoEscape, ObjectNode}
import org.apache.bcel.generic.{ANEWARRAY, ArrayType}
import sai.vm.Reference
import vm.Frame
import vm.interpreter.{Id, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object AnewarrayInterpreter extends InterpreterBuilder[ANEWARRAY] {

  override def apply(i: ANEWARRAY): InstructionInterpreter = {
    case frame @ Frame(method, cpg, stack, _, cg, _) =>
      val objectNode   = ObjectNode(Id(method, i))
      val arrayref     = Reference(new ArrayType(i.getType(cpg), 1), objectNode)
      val updatedStack = stack.pop.push(arrayref)
      val updatedCG =
        cg.addNode(objectNode)
          .updateEscapeState(objectNode -> NoEscape)
      frame.copy(stack = updatedStack, cg = updatedCG)
  }
}
