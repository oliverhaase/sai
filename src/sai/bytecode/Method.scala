package sai.bytecode

import scala.collection.mutable

import bytecode.BasicBlock
import cg.ConnectionGraph
import org.apache.bcel.Const
import org.apache.bcel.classfile.CodeException
import org.apache.bcel.classfile.Utility
import org.apache.bcel.generic.ConstantPoolGen
import org.apache.bcel.generic.InstructionList
import org.apache.bcel.generic.InstructionHandle
import org.apache.bcel.generic.ExceptionThrower
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.EntryPoint
import sai.bytecode.instruction.ExitPoint
import sai.bytecode.instruction.ControlFlowInstruction
import sai.vm.ObjectRef
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
    if ( isDefined )
      decorate(body(new InstructionList(bcelMethod.getCode.getCode).getInstructionHandles.toList))
    else
      Nil

  def exitPoint = instructions.last

  def entryPoint = instructions.head

  def firstInstruction = instructions(1)

  def isInsideTryBlock(instruction: Instruction) =
    instruction.pc.exists(pc => catchBlockRanges.exists(_.contains(pc)))

  private def catchBlockRanges: List[Range] =
    for (catchBlock <- bcelMethod.getCode.getExceptionTable.toList)
      yield Range(catchBlock.getStartPC, catchBlock.getEndPC)

  def basicBlocks = {
    val leaders = instructions.flatMap {
      case ep: EntryPoint => Some(ep)
      case i: ControlFlowInstruction => i.successors
      case _ => None
    }
    for (leader <- leaders.distinct)
      yield new BasicBlock(this, leader)
  }

  def lookup(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Instruction =
    instructions.find(_ encapsulates bcelInstruction)
      .getOrElse(throw new RuntimeException("instruction not found"))

  def lookup(bcelInstruction: org.apache.bcel.generic.Instruction): Instruction =
    instructions.find(_ encapsulates bcelInstruction)
      .getOrElse(throw new RuntimeException("instruction not found"))

  def getLineNumber(bcelInstruction: org.apache.bcel.generic.Instruction): Int = {
    lookup(bcelInstruction).pc
      .map(bcelMethod.getLineNumberTable.getSourceLine)
      .getOrElse(throw new RuntimeException("no PC for instruction found"))
  }

  private def argReferences(index: Int, bcelArgs: List[org.apache.bcel.generic.Type]): Map[Int, ObjectRef] =
    if ( bcelArgs == Nil )
      Map()
    else
      bcelArgs.head match {
        case basicType: org.apache.bcel.generic.BasicType =>
          argReferences(index + basicType.getSize, bcelArgs.tail)
        case referenceType: org.apache.bcel.generic.ReferenceType =>
          argReferences(index + 1, bcelArgs.tail) + (index -> ObjectRef(referenceType, "bla"))
      }

  val inputReferences: Map[Int, ObjectRef] =
    if ( bcelMethod.isStatic )
      argReferences(0, bcelMethod.getArgumentTypes.toList)
    else
      argReferences(1, bcelMethod.getArgumentTypes.toList) + (0 -> ObjectRef(null /*TODO insert correct class type here*/ , clazz.name))

  def maxLocals: Int = bcelMethod.getCode.getMaxLocals

  def name: String = bcelMethod.getName

  override def toString: String = name

  lazy val summary: ConnectionGraph = {
    val worklist = buildWorklist()
    var frame = Frame(this)

    // store the out state for each instruction (initialized with an empty connection graph)
    val outStates = mutable.Map.empty[Instruction, ConnectionGraph]
    outStates ++= instructions.map(instruction => (instruction, ConnectionGraph.empty())).toMap

    // we use an upper bound in case the out states don't converge (i.e. we never reach a fixed point)
    val upperBound = 10 * worklist.size
    var iteration = 0

    while (worklist.nonEmpty && iteration < upperBound) {
      iteration += 1

      val head = worklist.remove(0)
      val before = outStates(head)
      val inStates = head.predecessors.map(predecessor => outStates(predecessor))
      frame = head.transfer(frame, inStates)
      outStates(head) = frame.cg

      // out state changed => we have to recalculate the out states for each successor
      if (before != frame.cg) {
        // prepend successors so that operand stack stays correct
        worklist --= head.successors
        worklist.prependAll(head.successors)
      }
    }
    frame.cg
  }

  private def buildWorklist(): mutable.ListBuffer[Instruction] = {
    val worklist = mutable.ListBuffer.empty[Instruction]
    def visit(instruction: Instruction): Unit = {
      worklist += instruction
      val unvisited = instruction.successors.filterNot(worklist.contains)
      unvisited.foreach(visit)
    }
    visit(entryPoint)
    worklist
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




