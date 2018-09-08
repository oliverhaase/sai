package vm.interpreter

import sai.bytecode.instruction.Instruction
import vm.Frame
import vm.interpreter.impl._


trait InstructionInterpreter {

  protected[interpreter] def doInterpret(frame: Frame): Frame

  def interpret(frame: Frame): List[Frame] = doInterpret(frame) :: Nil
}

object InstructionInterpreter {

  def apply(instruction: Instruction): InstructionInterpreter = {
    val interpreter = lookupInterpreter(instruction.bcelInstruction.getInstruction)
    if (instruction.isExceptionTarget) {
      interpreter :: RaisePhantomExceptionDecorator(instruction)
    } else {
      interpreter
    }
  }

  private def lookupInterpreter(instruction: org.apache.bcel.generic.Instruction): InstructionInterpreter = instruction match {
    case i: org.apache.bcel.generic.AASTORE => AastoreInterpreter(i)
    case i: org.apache.bcel.generic.AALOAD => AaloadInterpreter(i)
    case i: org.apache.bcel.generic.ACONST_NULL => AconstNullInterpreter(i)
    case i: org.apache.bcel.generic.ALOAD => AloadInterpreter(i)
    case i: org.apache.bcel.generic.ANEWARRAY => AnewarrayInterpreter(i)
    case i: org.apache.bcel.generic.ASTORE => AstoreInterpreter(i)
    case i: org.apache.bcel.generic.ATHROW => AthrowInterpreter(i)
    case i: org.apache.bcel.generic.ARETURN => AreturnInterpreter(i)
    case i: org.apache.bcel.generic.DUP => DupInterpreter(i)
    case i: org.apache.bcel.generic.DUP_X1 => DupX1Interpreter(i)
    case i: org.apache.bcel.generic.DUP_X2 => DupX2Interpreter(i)
    case i: org.apache.bcel.generic.DUP2 => Dup2Interpreter(i)
    case i: org.apache.bcel.generic.DUP2_X1 => Dup2X1Interpreter(i)
    case i: org.apache.bcel.generic.DUP2_X2 => Dup2X2Interpreter(i)
    case i: org.apache.bcel.generic.GETFIELD => GetFieldInterpreter(i)
    case i: org.apache.bcel.generic.GETSTATIC => GetStaticInterpreter(i)
    case i: org.apache.bcel.generic.MULTIANEWARRAY => MultianewarrayInterpreter(i)
    case i: org.apache.bcel.generic.NEW => NewInterpreter(i)
    case i: org.apache.bcel.generic.PUTFIELD => PutFieldInterpreter(i)
    case i: org.apache.bcel.generic.PUTSTATIC => PutStaticInterpreter(i)
    case i: org.apache.bcel.generic.SWAP => SwapInterpreter(i)
    case i => TrivialTransferFunction(i)
  }

}