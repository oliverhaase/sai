package vm.interpreter.impl

import cg.{NoEscape, ObjectNode}
import org.apache.bcel.generic.{BasicType, LDC, ReferenceType}
import sai.vm.{DontCare, Reference}
import vm.Frame
import vm.interpreter.{Id, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object LdcInterpreter extends InterpreterBuilder[LDC] {

  override def apply(i: LDC): InstructionInterpreter = new InstructionInterpreter {
    override protected[interpreter] def doInterpret(frame: Frame): Frame = {
      val (newCG, newStack) = i.getType(frame.cpg) match {
        case objectType: ReferenceType =>
          val node = ObjectNode(Id(frame.method, i))
          val updatedCG = frame.cg.addNode(node).updateEscapeState(node -> NoEscape)
          val updatedStack = frame.stack.push(Reference(objectType, node))
          (updatedCG, updatedStack)
        case _: BasicType =>
          (frame.cg, frame.stack.push(DontCare, i.produceStack(frame.cpg)))
      }
      frame.copy(stack = newStack, cg = newCG)
    }
  }
}
