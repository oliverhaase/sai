package sai.bytecode.instruction

import bytecode.instruction.InstructionInterpreter
import cg.ConnectionGraph
import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import vm.Frame

class Instruction(protected val bcelInstruction: org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen,
    val method: Method) extends Ordered[Instruction] {

  final def pc: Option[Int] =
    if (bcelInstruction == null)
      None
    else Some(bcelInstruction.getPosition)

  def lineNumber = method.lineNumber(bcelInstruction)

  def length = bcelInstruction.getInstruction.getLength

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

  def prev: Instruction =
    if (bcelInstruction.getPrev == null)
      method.entryPoint
    else lookupInstruction(bcelInstruction.getPrev)

  def successors: List[Instruction] = {
    bcelInstruction.getInstruction match {
      case _: org.apache.bcel.generic.ATHROW if !method.exceptionInfo.isThrowInFinallyBlock(this) =>
        List(method.exitPoint)
      case _: org.apache.bcel.generic.ReturnInstruction =>
        List(method.exitPoint)
      case _ => List(next)
    }
  }

  final def predecessors: List[Instruction] =
    for (candidate <- method.instructions if candidate.successors.contains(this))
      yield candidate

  def isInsideTryBlock = method.exceptionInfo.isInsideTryBlock(this)

  def transfer(frame: Frame, inStates: Set[ConnectionGraph]): Frame = {
    val inState = inStates.reduce(_ merge _)
    val outState = InstructionInterpreter.handle(frame.copy(cg = inState), bcelInstruction.getInstruction)
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
    s"${bcelInstruction.getInstruction.getName} | pc = $pc | info = $info"
  }
  
  def print {
    val pos = if (bcelInstruction == null) " " else bcelInstruction.getPosition
    println(pos + ": " + toString)
  }

  override def compare(that: Instruction): Int = Ordering[Option[Int]].compare(pc, that.pc)
}

object Instruction {

  private[this] trait FindCatchLeaders extends Instruction {
    override def successors: List[Instruction] = {
      val catchLeaders =
        if (method.exceptionInfo.isLastTryInstruction(this))
          method.exceptionInfo.findCatchLeaders(this)
        else
          Nil
      super.successors ::: catchLeaders
    }
  }

  def apply(bcelInstruction : org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, method: Method) = 
    bcelInstruction.getInstruction match {
      case _: org.apache.bcel.generic.BranchInstruction => new ControlFlowInstruction(bcelInstruction, cpg, method)
      case _ => new Instruction(bcelInstruction, cpg, method) with FindCatchLeaders
    }
}

