package sai.bytecode.instruction

import org.apache.bcel.generic.ConstantPoolGen
import org.apache.bcel.generic.InstructionHandle
import sai.bytecode.Method
import sai.vm._

class Instruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, 
    method: Method) {
 
  protected def lookupInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    method lookup bcelInstruction
    
  def encapsulates(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    bcelInstruction == this.bcelInstruction

  def next: Instruction = 
      if (bcelInstruction.getNext == null) 
         method exitPoint 
      else lookupInstruction(bcelInstruction getNext)
  
  def successors: List[Instruction] = bcelInstruction.getInstruction match {
    case i: org.apache.bcel.generic.ReturnInstruction => 
        List(method exitPoint)
    case i: org.apache.bcel.generic.ATHROW => 
        List(method exitPoint)
    case _ => List(next)
  }

  def predecessors: Set[Instruction] = {
    val (predecessors, _) = method.instructions.span(_ != this)
    predecessors.toSet
  }

    
  def statesIn: Set[State] = 
    for (predecessor <- predecessors;
        predState <- predecessor.statesOut) yield predState
  
  protected def transferLocalVars(localVars: LocalVars) = localVars      
  protected def transferOpStack(opStack: OpStack) = opStack      
  protected def transferEdges(edges: FieldEdges) = edges      
        
  protected def transfer(state: State): State = 
    State(transferLocalVars(state.localVars), 
        transferOpStack(state.opStack), 
        transferEdges(state.edges))
 
  def statesOut: Set[State] = 
    for ( stateIn <- statesIn ) yield transfer(stateIn) 
  
  override def toString = bcelInstruction.getInstruction.getName
  
  def print {
    println("-" + toString + "-> " + statesOut)
  }
}

object Instruction {
  def apply(bcelInstruction : org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, method: Method) = 
    bcelInstruction getInstruction match {
      case bi: org.apache.bcel.generic.BranchInstruction => new ControlFlowInstruction(bcelInstruction, cpg, method)
      case _ => new Instruction(bcelInstruction, cpg, method)
    }
}

