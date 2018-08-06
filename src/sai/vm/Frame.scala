package vm

import cg.ConnectionGraph
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.util.Stack
import sai.vm.LocalVars
import sai.vm.Slot

case class Frame(method: Method, cpg: ConstantPoolGen, stack: Stack[Slot], localVars: LocalVars, cg: ConnectionGraph)

object Frame {
  def apply(method: Method): Frame = {
    val localVariables = LocalVars(method.maxLocals, method.inputReferences)
    Frame(method, method.cpg, OpStack(), localVariables, ConnectionGraph.empty())
  }
}

