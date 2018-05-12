package sai.bytecode.instruction

import scala.annotation.tailrec

import cg.ConnectionGraph
import cg.ObjectNode
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import sai.vm._

class Instruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen,
    method: Method) {

  final def pc: Option[Int] =
    if (bcelInstruction == null)
      None
    else Some(bcelInstruction.getPosition)

  protected def lookupInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    method lookup bcelInstruction

  def encapsulates(bcelInstruction: org.apache.bcel.generic.InstructionHandle) =
    bcelInstruction == this.bcelInstruction

  def next: Instruction = 
      if (bcelInstruction.getNext == null) 
         method.exitPoint
      else lookupInstruction(bcelInstruction.getNext)
  
  def successors: List[Instruction] = bcelInstruction.getInstruction match {
    case _: org.apache.bcel.generic.ExceptionThrower =>
      val catchInstructions = method.getCatchInstructions(bcelInstruction)
      (catchInstructions + next).toList
    case _ => List(next)
  }

  final def predecessors: Set[Instruction] = {
    method.instructions.filter(_.successors.contains(this)).toSet
  }

  def transfer(inStates: Set[ConnectionGraph]): ConnectionGraph = {
    val inState = inStates.fold(ConnectionGraph.empty())(_ merge _)
    val outState = inState
    outState
  }

  protected def transferLocalVars(localVars: LocalVars) = localVars      
  protected def transferOpStack(opStack: OpStack) = opStack      
  protected def transferEdges(edges: FieldEdges) = edges      
        
  protected def transfer(state: State): State = 
    State(transferLocalVars(state.localVars), 
        transferOpStack(state.opStack), 
        transferEdges(state.edges))

  override def toString = bcelInstruction.getInstruction.getName
  
  def print {
    println("-" + toString + "-> ")
  }
}

object Instruction {
  def apply(bcelInstruction : org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, method: Method) = 
    bcelInstruction.getInstruction match {
      case bi: org.apache.bcel.generic.BranchInstruction => new ControlFlowInstruction(bcelInstruction, cpg, method)
      case _ => new Instruction(bcelInstruction, cpg, method)
    }
}

