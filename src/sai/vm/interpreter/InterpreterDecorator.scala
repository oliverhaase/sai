package vm.interpreter

import cg.{GlobalEscape, LocalReferenceNode, NoEscape, ObjectNode}
import org.apache.bcel.generic.{ObjectType, ReferenceType, Type}
import sai.bytecode.Clazz
import sai.bytecode.instruction.Instruction
import sai.vm.{ObjectRef, OpStack}
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

        if (stack.depth == 1) {
          // there is already an exception object on top of the stack
          assert(stack.peek.isInstanceOf[ObjectRef])
          val objectRef = stack.peek.asInstanceOf[ObjectRef]
          val refType = objectRef.referenceType.asInstanceOf[ObjectType].getClassName
          val clazz = new Clazz(refType)
          assert(clazz.superClasses.contains(classOf[java.lang.Throwable]))
          otherInterpreter(frame)
        } else {
          // create exception object since stack is empty
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
          val updatedStack = OpStack(objectRef :: Nil)

          val updatedFrame = frame.copy(cg = updatedCG, stack = updatedStack)
          otherInterpreter(updatedFrame)



        }
    }
  }

}
