package vm.interpreter

import org.apache.bcel.generic.Instruction

private[interpreter] trait InterpreterBuilder[I <: Instruction] {
  def apply(i: I): InstructionInterpreter
}

