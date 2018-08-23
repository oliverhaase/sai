package vm.interpreter

import cg.{GlobalEscape, LocalReferenceNode, NoEscape, ObjectNode}
import org.apache.bcel.generic.{ReferenceType, Type}
import sai.bytecode.instruction.Instruction
import sai.vm.ObjectRef
import vm.Frame
import vm.interpreter.InstructionInterpreter.Interpreter


private[interpreter] sealed trait InterpreterDecorator {

  /**
    * Decorate an interpreter instance.
    *
    * @param interpreter New interpreter which will replace the existing one.
    */
  def ::(interpreter: Interpreter) = decorate(interpreter)

  /**
    * Replace a given interpreter with a new one.
    */
  def decorate: Interpreter => Interpreter

}

private[interpreter] object RaisePhantomExceptionDecorator {

  def apply(i: Instruction) = new InterpreterDecorator {
    override def decorate: (Interpreter) => Interpreter = otherInterpreter => {
      case frame@Frame(method, _, stack, _, cg) =>

        val objectNode = ObjectNode(Id(method, i.bcelInstruction.getInstruction))
        val referenceNode = LocalReferenceNode(Id(method, i.bcelInstruction.getInstruction))
        val className = i.getTargetExceptionType
        val clazz = Class.forName(className)
        val classType = Type.getType(clazz).asInstanceOf[ReferenceType]
        val objectRef = ObjectRef(classType, referenceNode)

        val updatedCG =
          cg
            .addNodes(referenceNode, objectNode)
            .addEdge(referenceNode -> objectNode)
            .updateEscapeState(referenceNode -> NoEscape)
            .updateEscapeState(objectNode -> GlobalEscape)
        val updatedStack = stack.push(objectRef)

        val updatedFrame = frame.copy(cg = updatedCG, stack = updatedStack)
        otherInterpreter(updatedFrame)
    }
  }

}
