package sai.bytecode

import org.apache.bcel.generic.ConstantPoolGen
import org.apache.bcel.generic.InstructionList
import org.apache.bcel.generic.InstructionHandle
import org.apache.bcel.classfile.JavaClass
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.EntryPoint
import sai.bytecode.instruction.ExitPoint

class Method (bcelMethod : org.apache.bcel.classfile.Method, cpg: ConstantPoolGen, clazz: Clazz) {  
  val isAbstract = bcelMethod isAbstract
  val isNative = bcelMethod isNative
  val isDefined = !isAbstract && !isNative
    
  private def body(bcelInstructions: List[InstructionHandle]) = 
     for ( bcelInstruction <- bcelInstructions ) 
       yield Instruction(bcelInstruction, cpg, this)
  
  private def decorate(body: List[Instruction]) = 
    new EntryPoint(this) :: body ::: List(new ExitPoint(this))
     
  val instructions: List[Instruction] = 
    if ( isDefined ) 
      decorate(body(new InstructionList(bcelMethod.getCode().getCode()).getInstructionHandles().toList))
    else
      Nil 
      
  def exitPoint = instructions last    
  
  def firstInstruction = instructions(1)
  
  def lookup(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Instruction = 
    instructions find (_ encapsulates bcelInstruction) match {
      case Some(i) => i
      case None => throw new RuntimeException("instruction not found")
    }
  
  
  def name = bcelMethod getName
  override def toString = name
  
  def summary = exitPoint statesOut
  
  def interpret {
      summary
      print
    }
  
  
  def print {
    println("." + toString)
    instructions.foreach(instruction => instruction print)
  }
  
}




