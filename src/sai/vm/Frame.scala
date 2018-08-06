package vm

import cg.ConnectionGraph
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.vm.{LocalVars, OpStack}

case class Frame(method: Method, cpg: ConstantPoolGen, stack: OpStack, localVars: LocalVars, cg: ConnectionGraph) {
  def merge(other: Frame): Frame = {
    val mergedStack = stack.merge(other.stack)
    val mergedLocalVars = localVars.merge(other.localVars)
    val mergedCG = cg.merge(other.cg)
    copy(stack = mergedStack, localVars = mergedLocalVars, cg = mergedCG)
  }
}

object Frame {
  def apply(method: Method): Frame = {
    val localVariables = LocalVars(method.maxLocals, method.inputReferences)
    Frame(method, method.cpg, OpStack(), localVariables, ConnectionGraph.empty())
  }
}

