package sai.bytecode

import bytecode.{BasicBlock, BasicBlocks, ExceptionInfo}
import cg._
import org.apache.bcel.generic.{ConstantPoolGen, InstructionHandle, InstructionList}
import sai.bytecode.instruction.{EntryPoint, ExitPoint, Instruction}
import sai.vm.Reference
import vm.Frame

class Method(bcelMethod: org.apache.bcel.classfile.Method, val cpg: ConstantPoolGen, val clazz: Clazz) {

  val isAbstract = bcelMethod.isAbstract
  val isNative = bcelMethod.isNative
  val isDefined = !isAbstract && !isNative

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
    instructions.find(predicate)
      .getOrElse(throw new RuntimeException("instruction not found"))

  def lineNumber(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Int = {
    val pos = lookup(bcelInstruction).pc.get
    bcelMethod.getLineNumberTable.getSourceLine(pos)
  }

  private def argReferences(index: Int, bcelArgs: List[org.apache.bcel.generic.Type]): Map[Int, Reference] =
    if (bcelArgs == Nil)
      Map()
    else
      bcelArgs.head match {
        case basicType: org.apache.bcel.generic.BasicType =>
          argReferences(index + basicType.getSize, bcelArgs.tail)
        case referenceType: org.apache.bcel.generic.ReferenceType =>
          argReferences(index + 1, bcelArgs.tail) + (index -> Reference(referenceType, ActualReferenceNode(this, index)))
      }

  val inputReferences: Map[Int, Reference] =
    if (bcelMethod.isStatic)
      argReferences(0, bcelMethod.getArgumentTypes.toList)
    else
      argReferences(1, bcelMethod.getArgumentTypes.toList) + (0 -> Reference(clazz.classType, ActualReferenceNode(this, 0)))

  def maxLocals: Int = bcelMethod.getCode.getMaxLocals

  def name: String = bcelMethod.getName

  override def toString: String = name

  lazy val summary: ConnectionGraph = {
    val initialFrame = Frame(this)

    val firstBasicBlock = controlFlowGraph.head
    val worklist = scala.collection.mutable.ListBuffer(firstBasicBlock)
    val outputFrames = scala.collection.mutable.Map.empty[BasicBlock, Frame]
    var iterations = 0
    val threshold = 20
    var outputFrame: Frame = null

    while (worklist.nonEmpty && iterations < threshold) {
      iterations += 1

      val block = worklist.remove(0)
      val inputFrames = for {
        predecessor <- block.predecessors
        frame <- outputFrames.get(predecessor)
      } yield frame

      val inputFrame = inputFrames match {
        case Nil => initialFrame
        case frames => frames.reduce(_ merge _)
      }
      outputFrame = block.interpret(inputFrame)

      val cgChanged = outputFrames.get(block).fold(true)(_.cg != outputFrame.cg)
      if (cgChanged) {
        outputFrames(block) = outputFrame
        worklist.appendAll(block.successors)
      }
    }

    if (iterations == threshold) {
      outputFrame.cg.bottomSolution
    } else {
      outputFrame.cg
    }
  }

  def interpret {
    println(summary)
    print
  }

  def print {
    println("." + toString + " " + inputReferences)
    instructions.foreach(instruction => instruction.print)
  }
}




