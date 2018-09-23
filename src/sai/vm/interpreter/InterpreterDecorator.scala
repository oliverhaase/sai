package vm.interpreter

import cg.{GlobalEscape, ObjectNode}
import org.apache.bcel.generic.{ObjectType, ReferenceType, Type}
import sai.bytecode.Clazz
import sai.bytecode.instruction.Instruction
import sai.vm.{OpStack, Reference}
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
    override def decorate: (InstructionInterpreter) => InstructionInterpreter =
      originalInterpreter =>
        new InstructionInterpreter {
          override protected[interpreter] def doInterpret(frame: Frame): Frame = {
            val Frame(method, _, _, _, cg, _) = frame

            val objectNode = ObjectNode(Id(method, i.bcelInstruction.getInstruction))
            val className  = i.getTargetExceptionType
            val clazz      = Class.forName(className)
            val classType  = Type.getType(clazz).asInstanceOf[ReferenceType]
            val objectRef  = Reference(classType, objectNode)

            val updatedCG =
              cg.addNodes(objectNode)
                .updateEscapeState(objectNode -> GlobalEscape)
            val updatedStack = OpStack(objectRef :: Nil)
            val updatedFrame = frame.copy(cg = updatedCG, stack = updatedStack)
            originalInterpreter.doInterpret(updatedFrame)
          }
      }
  }
}
