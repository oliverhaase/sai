package vm.interpreter.impl

import cg.{NoEscape, ObjectNode}
import org.apache.bcel.generic.{ArrayType, MULTIANEWARRAY}
import sai.vm.{OpStack, Reference}
import vm.Frame
import vm.interpreter.{Helper, Id, InstructionInterpreter, InterpreterBuilder}

private[interpreter] object MultianewarrayInterpreter extends InterpreterBuilder[MULTIANEWARRAY] {

  override def apply(i: MULTIANEWARRAY): InstructionInterpreter = {
    case frame @ Frame(method, cpg, stack, _, cg) =>
      val dimensions = i.getDimensions
      val slots      = stack.pop(dimensions).elements

      var arrayNode = ObjectNode(Id(method, i))
      val objectref = Reference(new ArrayType(i.getType(cpg), dimensions), arrayNode)
      val newCG =
        cg.addNode(arrayNode)
          .updateEscapeState(arrayNode -> NoEscape)

      var update    = Helper.getOrCreateFieldNode(newCG, arrayNode, Helper.arrayFieldName)
      var fieldNode = update._1
      var updatedCG = update._2

      (0 until dimensions - 1).foreach { dimension =>
        arrayNode = ObjectNode(Id(method, i) + dimension)
        updatedCG = updatedCG
          .addNode(arrayNode)
          .updateEscapeState(arrayNode -> NoEscape)
          .addEdge(fieldNode -> arrayNode)
        update = Helper.getOrCreateFieldNode(updatedCG, arrayNode, Helper.arrayFieldName)
        fieldNode = update._1
        updatedCG = update._2
      }
      val updatedStack = OpStack(objectref :: slots)
      frame.copy(stack = updatedStack, cg = updatedCG)
  }

}
