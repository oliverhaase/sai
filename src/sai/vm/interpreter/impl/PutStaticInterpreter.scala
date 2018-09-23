package vm.interpreter.impl

import cg.{GlobalEscape, StaticReferenceNode}
import org.apache.bcel.generic.{PUTSTATIC, ReferenceType}
import sai.vm.{Null, Reference}
import vm.Frame
import vm.interpreter.{InstructionInterpreter, InterpreterBuilder}

private[interpreter] object PutStaticInterpreter extends InterpreterBuilder[PUTSTATIC] {

  override def apply(i: PUTSTATIC): InstructionInterpreter = {
    case frame@Frame(_, cpg, stack, _, cg, _) =>
      val value = stack.peek
      val updatedStack = stack.pop
      val updatedCG = i.getReferenceType(cpg) match {
        case referenceType: ReferenceType =>
          value match {
            case Reference(_, q) =>
              val staticReferenceNode = StaticReferenceNode(referenceType, i.getFieldName(cpg))
              val updatedCG =
                cg.addNode(staticReferenceNode)
                  .addEdge(staticReferenceNode -> q)
                  .updateEscapeState(staticReferenceNode -> GlobalEscape)
              updatedCG
            case _ =>
              cg
          }
        case _ =>
          cg
      }
      frame.copy(stack = updatedStack, cg = updatedCG)
  }
}
