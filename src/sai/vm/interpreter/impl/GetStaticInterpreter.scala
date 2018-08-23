package vm.interpreter.impl

import cg.{GlobalEscape, StaticReferenceNode}
import org.apache.bcel.generic.{BasicType, GETSTATIC, ReferenceType}
import sai.vm.{DontCare, ObjectRef}
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.InstructionInterpreter.Interpreter

private[interpreter] object GetStaticInterpreter extends InstructionInterpreter[GETSTATIC] {
  
  override def apply(i: GETSTATIC): Interpreter = {
    case frame@Frame(_, cpg, stack, _, cg) =>
      i.getFieldType(cpg) match {
        case referenceType: ReferenceType =>
          val staticReferenceNode = StaticReferenceNode(referenceType, i.getIndex)
          val reference = ObjectRef(referenceType, staticReferenceNode)
          val updatedStack = stack.push(reference)
          val updatedCG =
            cg.addNode(staticReferenceNode)
              .updateEscapeState(staticReferenceNode -> GlobalEscape)
          frame.copy(stack = updatedStack, cg = updatedCG)
        case _: BasicType =>
          val updatedStack = stack.push(DontCare)
          frame.copy(stack = updatedStack)
      }
  }
}
