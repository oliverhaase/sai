package sai.bytecode.instruction

import bytecode.instruction.InstructionHandler
import cg.ConnectionGraph
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import vm.Frame

class Instruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen,
    val method: Method) {

  final def pc: Option[Int] =
    if (bcelInstruction == null)
      None
    else Some(bcelInstruction.getPosition)

  def lineNumber = method.getLineNumber(bcelInstruction.getInstruction)

  protected def lookupInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    method lookup bcelInstruction

  def encapsulates(bcelInstruction: org.apache.bcel.generic.InstructionHandle) =
    bcelInstruction == this.bcelInstruction

  def encapsulates(bcelInstruction: org.apache.bcel.generic.Instruction) =
    this.bcelInstruction != null && bcelInstruction == this.bcelInstruction.getInstruction

  def next: Instruction = 
      if (bcelInstruction.getNext == null) 
         method.exitPoint
      else lookupInstruction(bcelInstruction.getNext)
  
  def successors: List[Instruction] = bcelInstruction.getInstruction match {
    case _: org.apache.bcel.generic.RETURN => List(method.exitPoint)
    case _ => List(next)
  }

  private def isInsideTryBlock = method.isInsideTryBlock(this)

  final def predecessors: Set[Instruction] =
    for (candidate <- method.instructions.toSet if candidate.successors.contains(this))
      yield candidate

  def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = {
    val inState = inStates.reduce(_ merge _)
    val outState = InstructionHandler.handle(frame.copy(cg = inState), bcelInstruction.getInstruction)
    outState
  }

  override def toString = {
    val info = bcelInstruction.getInstruction match {
      case i: org.apache.bcel.generic.StoreInstruction =>
        Map("index" -> i.getIndex, "tag" -> i.getCanonicalTag)
      case i: org.apache.bcel.generic.LoadInstruction =>
        Map("index" -> i.getIndex, "tag" -> i.getCanonicalTag)
      case i: org.apache.bcel.generic.InvokeInstruction =>
        Map("class name" -> i.getClassName(cpg), "method name" -> i.getMethodName(cpg))
      case i: org.apache.bcel.generic.FieldInstruction =>
        Map("field name" -> i.getFieldName(cpg))
      case i: org.apache.bcel.generic.RETURN =>
        Map("return type" -> i.getType)
      case i: org.apache.bcel.generic.NEW =>
        Map("type" -> i.getLoadClassType(cpg))
      case i: org.apache.bcel.generic.DUP =>
        Map("length" -> i.getLength)
      case _ => ""
    }
    bcelInstruction.getInstruction.getName + " -> info: " + info.toString
  }
  
  def print {
    val pos = if (bcelInstruction == null) " " else bcelInstruction.getPosition
    println(pos + ": " + toString)
  }
}

object Instruction {
  def apply(bcelInstruction : org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, method: Method) = 
    bcelInstruction.getInstruction match {
      case bi: org.apache.bcel.generic.BranchInstruction => new ControlFlowInstruction(bcelInstruction, cpg, method)
      case _ => new Instruction(bcelInstruction, cpg, method)
    }
}

