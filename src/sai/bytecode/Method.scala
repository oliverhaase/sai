package sai.bytecode

import scala.annotation.tailrec
import scala.collection.mutable

import bytecode.BasicBlock
import bytecode.ExceptionInfo
import bytecode.BasicBlocks
import cg.ConnectionGraph
import cg.ObjectNode
import org.apache.bcel.generic.ConstantPoolGen
import org.apache.bcel.generic.InstructionList
import org.apache.bcel.generic.InstructionHandle
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.EntryPoint
import sai.bytecode.instruction.ExitPoint
import sai.vm.ObjectRef
import vm.Frame
import vm.interpreter.InstructionInterpreter

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
    if ( isDefined )
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

  def lookup(pc: Int): Instruction =
    lookup(_.pc contains pc)

  def lookup(predicate: Instruction => Boolean): Instruction =
    instructions.find(predicate)
      .getOrElse(throw new RuntimeException("instruction not found"))

  def lineNumber(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Int = {
    val pos = lookup(bcelInstruction).pc.get
    bcelMethod.getLineNumberTable.getSourceLine(pos)
  }

  private def argReferences(index: Int, bcelArgs: List[org.apache.bcel.generic.Type]): Map[Int, ObjectRef] =
    if ( bcelArgs == Nil )
      Map()
    else
      bcelArgs.head match {
        case basicType: org.apache.bcel.generic.BasicType =>
          argReferences(index + basicType.getSize, bcelArgs.tail)
        case referenceType: org.apache.bcel.generic.ReferenceType =>
          argReferences(index + 1, bcelArgs.tail) + (index -> ObjectRef(referenceType, clazz.name, ObjectNode("unique")))
      }

  val inputReferences: Map[Int, ObjectRef] =
    if ( bcelMethod.isStatic )
      argReferences(0, bcelMethod.getArgumentTypes.toList)
    else
      argReferences(1, bcelMethod.getArgumentTypes.toList) + (0 -> ObjectRef(null /*TODO insert correct class type here*/ , clazz.name, ObjectNode("unique")))

  def maxLocals: Int = bcelMethod.getCode.getMaxLocals

  def name: String = bcelMethod.getName

  override def toString: String = name

  lazy val summary: ConnectionGraph = {
    var frame = Frame(this)
    val firstBasicBlock = controlFlowGraph.head
    val worklist = scala.collection.mutable.Buffer[BasicBlock](firstBasicBlock)
    val outState = scala.collection.mutable.Map[BasicBlock, ConnectionGraph](firstBasicBlock -> ConnectionGraph.empty())

    // we use an upper bound in case the out states don't converge (i.e. we never reach a fixed point)
    val upperBound = 10
    var iteration = 0

    while (worklist.nonEmpty && iteration < upperBound) {
      iteration += 1

      val basicBlock = worklist.remove(0)
      val cgBefore = outState(basicBlock)
      val outStates = basicBlock.predecessors.map(outState)
      val inState = outStates.fold(ConnectionGraph.empty())(_ merge _)
      frame = frame.copy(cg = inState)
      frame = basicBlock.instructions.foldLeft(frame)((frame, i) => i.interpret(frame))
      val cgAfter = frame.cg
      outState(basicBlock) = cgAfter

      // out state changed => we have to recalculate the out states for each successor
      if (cgBefore != cgBefore) {
        // prepend successors so that operand stack stays correct
        worklist --= basicBlock.successors
        worklist.prependAll(basicBlock.successors)
      }
    }
    frame.cg
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




