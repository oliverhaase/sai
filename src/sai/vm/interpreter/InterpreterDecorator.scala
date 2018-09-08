package vm.interpreter

import cg.{GlobalEscape, LocalReferenceNode, NoEscape, ObjectNode}
import org.apache.bcel.generic.{ObjectType, ReferenceType, Type}
import sai.bytecode.Clazz
import sai.bytecode.instruction.Instruction
import sai.vm.{Reference, OpStack}
import vm.Frame


private[interpreter] sealed trait InterpreterDecorator {

  /**
    * Decorate an interpreter instance.
    *
    * @param interpreter New interpreter which will replace the existing one.
    */
  def ::(interpreter: InstructionInterpreter) = decorate(interpreter)

  /**
    * Replace a given interpreter with a new one.
    */
  def decorate: InstructionInterpreter => InstructionInterpreter

}

private[interpreter] object RaisePhantomExceptionDecorator {

  def apply(i: Instruction) = new InterpreterDecorator {
    override def decorate: (InstructionInterpreter) => InstructionInterpreter = originalInterpreter => new InstructionInterpreter {
      override protected[interpreter] def doInterpret(frame: Frame): Frame = {
        frame match {
          case frame@Frame(method, _, stack, _, cg) =>

            if (stack.depth == 1) {
              // there is already an exception object on top of the stack
              assert(stack.peek.isInstanceOf[Reference])
              val objectRef = stack.peek.asInstanceOf[Reference]
              val refType = objectRef.referenceType.asInstanceOf[ObjectType].getClassName
              val clazz = new Clazz(refType)
              assert(clazz.superClasses.contains(classOf[java.lang.Throwable]))
              originalInterpreter.doInterpret(frame)
            } else {
              // create exception object since stack is empty
              val objectNode = ObjectNode(Id(method, i.bcelInstruction.getInstruction))
              val className = i.getTargetExceptionType
              val clazz = Class.forName(className)
              val classType = Type.getType(clazz).asInstanceOf[ReferenceType]
              val objectRef = Reference(classType, objectNode)

              val updatedCG =
                cg
                  .addNodes(objectNode)
                  .updateEscapeState(objectNode -> GlobalEscape)
              val updatedStack = OpStack(objectRef :: Nil)

              val updatedFrame = frame.copy(cg = updatedCG, stack = updatedStack)
              originalInterpreter.doInterpret(updatedFrame)
            }
        }
      }
    }
  }
}
