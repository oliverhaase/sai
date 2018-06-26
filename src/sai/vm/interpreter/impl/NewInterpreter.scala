package vm.interpreter.impl

import cg.ObjectNode
import sai.vm.ObjectRef
import vm.Frame
import vm.interpreter.InstructionInterpreter
import vm.interpreter.Id

private[interpreter] object NewInterpreter extends InstructionInterpreter[org.apache.bcel.generic.NEW] {
  override def apply(i: org.apache.bcel.generic.NEW): Frame => Frame = {
    case frame @ Frame(_, stack, cg, _) =>
      val id = Id(i)
      val objectType = i.getLoadClassType(frame.cpg)
      val objectNode = ObjectNode(id)
      val objectRef = ObjectRef(objectType, id, objectNode)

      val updatedCG = cg.addNode(objectNode)
      val updatedStack = stack.push(objectRef)
      val updatedFrame = frame.copy(opStack = updatedStack, cg = updatedCG)
      updatedFrame
  }
}