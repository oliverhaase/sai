package vm

import cg.ConnectionGraph
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.util.Stack
import sai.vm.LocalVars
import sai.vm.Slot


case class Frame(method: Method, opStack: Stack[Slot], cg: ConnectionGraph, localVars: LocalVars) {
  def cpg: ConstantPoolGen = method.cpg
}

object Frame {
  def apply(method: Method): Frame = {
    val localVariables = LocalVars(method.maxLocals, method.inputReferences)
    Frame(method, Stack[Slot](), ConnectionGraph.empty(), localVariables)
  }
}

