package vm.interpreter

import vm.Frame
import vm.interpreter.impl._

private[interpreter] trait InstructionInterpreter[I] {
  def apply(i: I): Frame => Frame
}

object InstructionInterpreter {

  def apply(instruction: org.apache.bcel.generic.Instruction): Frame => Frame = {
    val interpreter = instruction match {
      case i: org.apache.bcel.generic.ACONST_NULL => AconstNullInterpreter(i)
      case i: org.apache.bcel.generic.ALOAD => AloadInterpreter(i)
      case i: org.apache.bcel.generic.ASTORE => AstoreInterpreter(i)
      case i: org.apache.bcel.generic.DUP => DupInterpreter(i)
      case i: org.apache.bcel.generic.GETFIELD => GetFieldInterpreter(i)
      case i: org.apache.bcel.generic.GETSTATIC => GetStaticInterpreter(i)
      case i: org.apache.bcel.generic.NEW => NewInterpreter(i)
      case i: org.apache.bcel.generic.PUTFIELD => PutFieldInterpreter(i)
      case i: org.apache.bcel.generic.PUTSTATIC => PutStaticInterpreter(i)
      case i => TrivialTransferFunction(i)
    }
    interpreter
  }
}