package vm.interpreter

import sai.bytecode.Method


object Id {

  type Id = String

  def apply(method: Method, i: org.apache.bcel.generic.Instruction): Id = {
    method.lookup(i).id
  }

}
