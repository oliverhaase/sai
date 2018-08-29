package sai.bytecode

import bytecode.{BasicBlock, BasicBlocks, ExceptionInfo}
import cg._
import implicits.MutableSetExtensions.convert
import org.apache.bcel.generic.{ConstantPoolGen, InstructionHandle, InstructionList}
import sai.bytecode.instruction.{EntryPoint, ExitPoint, Instruction}
import sai.vm.ObjectRef
import vm.Frame

class Method(bcelMethod: org.apache.bcel.classfile.Method, val cpg: ConstantPoolGen, val clazz: Clazz) {

  val isAbstract = bcelMethod.isAbstract
  val isNative = bcelMethod.isNative
  val isDefined = !isAbstract && !isNative

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
    instructions.find(predicate)
      .getOrElse(throw new RuntimeException("instruction not found"))

  def lineNumber(bcelInstruction: org.apache.bcel.generic.InstructionHandle): Int = {
    val pos = lookup(bcelInstruction).pc.get
    bcelMethod.getLineNumberTable.getSourceLine(pos)
  }

  private def argReferences(index: Int, bcelArgs: List[org.apache.bcel.generic.Type]): Map[Int, ObjectRef] =
    if (bcelArgs == Nil)
      Map()
    else
      bcelArgs.head match {
        case basicType: org.apache.bcel.generic.BasicType =>
          argReferences(index + basicType.getSize, bcelArgs.tail)
        case referenceType: org.apache.bcel.generic.ReferenceType =>
          argReferences(index + 1, bcelArgs.tail) + (index -> ObjectRef(referenceType, ActualReferenceNode(this, index)))
      }

  val inputReferences: Map[Int, ObjectRef] =
    if (bcelMethod.isStatic)
      argReferences(0, bcelMethod.getArgumentTypes.toList)
    else
      argReferences(1, bcelMethod.getArgumentTypes.toList) + (0 -> ObjectRef(clazz.classType, ActualReferenceNode(this, 0)))

  def maxLocals: Int = bcelMethod.getCode.getMaxLocals

  def name: String = bcelMethod.getName

  override def toString: String = name

  lazy val summary: ConnectionGraph = {

    // The calculation of the summary information starts with the first basic block in the control flow graph.
    val entryBlock = controlFlowGraph.head
    // We use a worklist in which we store we still need to interpret.
    val worklist = scala.collection.mutable.Set(entryBlock)

    // We store all output frames of each interpreted basic block.
    val outputFrames = scala.collection.mutable.Map.empty[BasicBlock, Set[Frame]]

    // A basic block is interpreted a maximum of 'threshold' times.
    // The algorithm terminates prematurely if the limit for a block has been reached.
    val iterations = scala.collection.mutable.Map.empty[BasicBlock, Int]
    val threshold = 10
    var reachedThreshold = false

    while (worklist.nonEmpty && !reachedThreshold) {
      // Pick and remove any block from the worklist.
      val currentBlock = worklist.removeAny()

      val inputFrames = currentBlock.predecessors match {
        case Nil => Set(Frame(this))
        case ps => ps.flatMap(outputFrames.getOrElse(_, Set.empty)).toSet
      }

      // Merge connection graphs of each ingoing frame.
      val inState = inputFrames.map(_.cg).reduce(_ merge _)

      // Interpret each input frame.
      val interpretedFrames = for {
        inputFrame <- inputFrames
        frameToInterpret = inputFrame.copy(cg = inState)
        interpretedFrame = currentBlock.interpret(frameToInterpret)
      } yield interpretedFrame

      // There is a change if the output frames before the interpretation are different from those after the interpretation.
      val framesChanged = outputFrames.get(currentBlock) != Some(interpretedFrames)
      if (framesChanged) {
        // Store the interpreted frames as output frames for the current block.
        outputFrames(currentBlock) = interpretedFrames
        // Add all successor blocks to the worklist since they may also change in the next iteration.
        worklist ++= currentBlock.successors
        // Increment the iteration counter for the block.
        iterations(currentBlock) = iterations.getOrElse(currentBlock, 0) + 1
        // Check if we reached the threshold for the current block.
        reachedThreshold = iterations(currentBlock) == threshold
      }
    }

    // Get all connection graphs of all output frames and merge them to build the summary graph.
    val frames = outputFrames.values.flatten
    val summary = frames.map(_.cg).reduce(_ merge _)

    if (reachedThreshold) {
      // If we reached the threshold, then we use the bottom solution (i.e. mark all object nodes as global escape)
      summary.bottomSolution
    } else {
      // If we did not reach the threshold, we perform the reachability analysis in order to update the escape states of the nodes.
      summary.performReachabilityAnalysis
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




