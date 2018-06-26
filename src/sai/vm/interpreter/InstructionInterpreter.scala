package vm.interpreter

import vm.Frame
import vm.interpreter.impl.NewInterpreter
import vm.interpreter.impl.TrivialTransferFunction
import vm.interpreter.impl.AloadInterpreter
import vm.interpreter.impl.AstoreInterpreter
import vm.interpreter.impl.DupInterpreter
import vm.interpreter.impl.GetFieldInterpreter
import vm.interpreter.impl.InvokeSpecialInterpreter
import vm.interpreter.impl.PutFieldInterpreter

private[interpreter] trait InstructionInterpreter[I] {
  def apply(i: I): Frame => Frame
}

object InstructionInterpreter {

  def apply(instruction: org.apache.bcel.generic.Instruction): Frame => Frame = {
    val interpreter = instruction match {
      case i: org.apache.bcel.generic.ALOAD => AloadInterpreter(i)
      case i: org.apache.bcel.generic.ASTORE => AstoreInterpreter(i)
      case i: org.apache.bcel.generic.DUP => DupInterpreter(i)
      case i: org.apache.bcel.generic.GETFIELD => GetFieldInterpreter(i)
      case i: org.apache.bcel.generic.INVOKESPECIAL => InvokeSpecialInterpreter(i)
      case i: org.apache.bcel.generic.NEW => NewInterpreter(i)
      case i: org.apache.bcel.generic.PUTFIELD => PutFieldInterpreter(i)
      case i => TrivialTransferFunction(i)
    }
    interpreter
  }
}