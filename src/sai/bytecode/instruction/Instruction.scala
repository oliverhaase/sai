package sai.bytecode.instruction

import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.vm.State

class Instruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, 
    method: Method) {
 
  private def lookupInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    method lookup bcelInstruction
    
  def encapsulates(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    bcelInstruction == this.bcelInstruction

  def next: Option[Instruction] = 
      if (bcelInstruction.getNext == null) 
         Some(method exitPoint) 
      else lookupInstruction(bcelInstruction getNext)
  
  def predecessors: Set[Instruction] = Set()
    
  def statesIn: Set[State] = 
    for (predecessor <- predecessors;
        predState <- predecessor.statesOut) yield predState
  
  private def transfer(states: Set[State]) = states 
 
  def statesOut = transfer(statesIn) 
  
  override def toString = bcelInstruction.getInstruction.getName
  
  def print {
    println("-" + toString + "-> " + next)
  }
}

object Instruction {
  def apply(bcelInstruction : org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, method: Method) = 
    bcelInstruction getInstruction match {
      case bi: org.apache.bcel.generic.BranchInstruction => new ControlFlowInstruction(bcelInstruction, cpg, method)
      case _ => new Instruction(bcelInstruction, cpg, method)
    }
    
    
}

