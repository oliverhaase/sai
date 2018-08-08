package sai.bytecode.instruction

import org.apache.bcel.generic.ConstantPoolGen
import sai.bytecode.Method
import vm.Frame
import vm.interpreter.InstructionInterpreter

class Instruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen,
                  val method: Method) extends Ordered[Instruction] {

  def id: String = buildId(bcelInstruction.getPosition.toString)

  protected def buildId(position: String) = s"${method.clazz.name}.${method.name},position=$position"

  final def pc: Option[Int] =
    if (bcelInstruction == null)
      None
    else Some(bcelInstruction.getPosition)

  def lineNumber = method.lineNumber(bcelInstruction)

  protected def lookupInstruction(bcelInstruction: org.apache.bcel.generic.InstructionHandle) = 
    method lookup bcelInstruction

  def encapsulates(bcelInstruction: org.apache.bcel.generic.InstructionHandle) =
    bcelInstruction == this.bcelInstruction

  def encapsulates(bcelInstruction: org.apache.bcel.generic.Instruction) =
    this.bcelInstruction != null && this.bcelInstruction.getInstruction.eq(bcelInstruction)

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
      case _: org.apache.bcel.generic.ATHROW =>
        List(method.exitPoint)
      case _: org.apache.bcel.generic.ReturnInstruction =>
        List(method.exitPoint)
      case _ => List(next)
    }
  }

  def interpret(frame: Frame): Frame = {
    if (bcelInstruction == null)
      frame
    else {
      val interpreter = InstructionInterpreter(this.bcelInstruction.getInstruction)
      interpreter(frame)
    }
  }

  final def predecessors: List[Instruction] =
    for (candidate <- method.instructions if candidate.successors.contains(this))
      yield candidate

  def isInTryRange = method.exceptionInfo.isInTryRange(this)

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

  def isExceptionTarget = method.exceptionInfo.isExceptionTarget(this)

  def getTargetExceptionType = method.exceptionInfo.getTargetExceptionType(this)
}

object Instruction {

  def apply(bcelInstruction : org.apache.bcel.generic.InstructionHandle, cpg: ConstantPoolGen, method: Method) =
    bcelInstruction.getInstruction match {
      case _: org.apache.bcel.generic.BranchInstruction =>
        new ControlFlowInstruction(bcelInstruction, cpg, method) with FindTargetSuccessors
      case _ =>
        new Instruction(bcelInstruction, cpg, method) with FindTargetSuccessors
    }

  trait FindTargetSuccessors extends Instruction {
    override def successors: List[Instruction] = {
      super.successors ::: method.exceptionInfo.findTargetSuccessors(this)
    }
  }

}

