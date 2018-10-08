package sai.bytecode

import bytecode._
import cg._
import org.apache.bcel.generic.{ConstantPoolGen, InstructionHandle, InstructionList}
import sai.bytecode.instruction.{EntryPoint, ExitPoint, Instruction}
import sai.vm.Reference
import vm.{Frame, IntraproceduralAnalysis, NonRecursiveSummary}

class Method(bcelMethod: org.apache.bcel.classfile.Method,
             val cpg: ConstantPoolGen,
             val clazz: Clazz) {

  val isAbstract = bcelMethod.isAbstract
  val isNative   = bcelMethod.isNative
  val isDefined  = !isAbstract && !isNative && bcelMethod.getCode != null
  val isPublic   = bcelMethod.isPublic
  val isInstanceMethod = !bcelMethod.isStatic

  def id = s"${clazz.name}:$name"

  private def body(bcelInstructions: List[InstructionHandle]) =
    for (bcelInstruction <- bcelInstructions)
      yield Instruction(bcelInstruction, cpg, this)

  private def decorate(body: List[Instruction]) =
    new EntryPoint(this) :: body ::: List(new ExitPoint(this))

  val instructions: List[Instruction] =
    if (isDefined)
      decorate(body(new InstructionList(bcelMethod.getCode.getCode).getInstructionHandles.toList))
    else
      Nil

  def exitPoint = instructions.last

  def entryPoint = instructions.head

  def firstInstruction = instructions(1)

  def lastInstruction = instructions(instructions.length - 2)

  lazy val controlFlowGraph: List[BasicBlock] = BasicBlocks(this)

  lazy val exceptionInfo = ExceptionInfo(this, bcelMethod.getCode.getExceptionTable.toList)

  def lookup(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Instruction =
    lookup(_ encapsulates bcelInstruction)

  def lookup(bcelInstruction: org.apache.bcel.generic.Instruction): Instruction =
    lookup(_ encapsulates bcelInstruction)

  def lookup(pc: Int): Instruction =
    lookup(_.pc contains pc)

  def lookup(predicate: Instruction => Boolean): Instruction =
    instructions
      .find(predicate)
      .getOrElse(throw new RuntimeException("instruction not found"))

  def lineNumber(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Int = {
    val pos = lookup(bcelInstruction).pc.get
    bcelMethod.getLineNumberTable.getSourceLine(pos)
  }

  private def argReferences(index: Int,
                            bcelArgs: List[org.apache.bcel.generic.Type]): Map[Int, Reference] =
    if (bcelArgs == Nil)
      Map()
    else
      bcelArgs.head match {
        case basicType: org.apache.bcel.generic.BasicType =>
          argReferences(index + basicType.getSize, bcelArgs.tail)
        case referenceType: org.apache.bcel.generic.ReferenceType =>
          argReferences(index + 1, bcelArgs.tail) + (index -> Reference(referenceType,
                                                                        ActualReferenceNode(this,
                                                                                            index)))
      }

  val inputReferences: Map[Int, Reference] =
    if (bcelMethod.isStatic)
      argReferences(0, bcelMethod.getArgumentTypes.toList)
    else
      argReferences(1, bcelMethod.getArgumentTypes.toList) + (0 -> Reference(
        clazz.classType,
        ActualReferenceNode(this, 0)))

  def maxLocals: Int = bcelMethod.getCode.getMaxLocals

  def name: String = bcelMethod.getName

  override def toString: String = id

  lazy val callGraph = CallGraph(this)

  lazy val nonRecursiveSummary = NonRecursiveSummary(this)

  lazy val summary =
    IntraproceduralAnalysis(Frame(this), controlFlowGraph, _.successors, _.predecessors)

  def interpret {
    println(summary)
    print
  }

  def print {
    println("." + toString + " " + inputReferences)
    instructions.foreach(instruction => instruction.print)
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case m: Method if m.id == id => true
      case _                       => false
    }
  }

  override def hashCode(): Int = id.hashCode

  def signature = bcelMethod.getSignature
}
