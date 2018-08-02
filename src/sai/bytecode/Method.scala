package sai.bytecode

import bytecode.{BasicBlock, BasicBlocks, ExceptionInfo}
import cg._
import org.apache.bcel.generic.{ConstantPoolGen, InstructionHandle, InstructionList}
import sai.bytecode.instruction.{EntryPoint, ExitPoint, Instruction}
import sai.vm.{LocalVars, Reference}
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

  private def transferActuals = {

    val actualReferences = inputReferences.collect {
      case (index, ref@Reference(_, _: ActualReferenceNode)) => index -> ref
    }

    val phantomReferences = (for {
      (index, Reference(actualType, actualNode: ActualReferenceNode)) <- actualReferences
      phantomNode = PhantomReferenceNode(actualNode)
      phantomReference = Reference(actualType, phantomNode)
    } yield index -> phantomReference).toMap

    val localReferences = (for {
      (index, Reference(actualType, actualNode: ActualReferenceNode)) <- actualReferences
      localNode = LocalReferenceNode(actualNode)
      localReference = Reference(actualType, localNode)
    } yield index -> localReference).toMap

    val deferredEdges: Set[Edge] = (for {
      (Reference(_, localNode: LocalReferenceNode), Reference(_, phantomNode: PhantomReferenceNode)) <- localReferences.values.zip(phantomReferences.values)
      deferredEdge = DeferredEdge(localNode -> phantomNode)
    } yield deferredEdge).toSet

    (actualReferences, phantomReferences, localReferences, deferredEdges)
  }

  private def prepareInitialFrame: Frame = {
    val (actualReferences, phantomReferences, localReferences, edges) = transferActuals
    val localVars = LocalVars(maxLocals, inputReferences ++ localReferences)

    val localEscapes = (for {
      Reference(_, localNode) <- localReferences.values
    } yield localNode -> NoEscape).toList

    val actualEscapes = (for {
      Reference(_, actualNode) <- actualReferences.values
    } yield actualNode -> ArgEscape).toList

    val escapes = localEscapes ++ actualEscapes

    val phantomNodes = (for {
      Reference(_, phantomNode) <- phantomReferences.values
    } yield phantomNode).toSet

    val localNodes = (for {
      Reference(_, localNode) <- localReferences.values
    } yield localNode).toSet

    val nodes = phantomNodes ++ localNodes

    val cg = ConnectionGraph.apply(nodes, edges, escapes.toMap)
    val initialFrame = Frame(this).copy(cg = cg, localVars = localVars)
    initialFrame
  }

  lazy val summary: ConnectionGraph = {
    val initialFrame = prepareInitialFrame

    val firstBasicBlock = controlFlowGraph.head

    val basicBlock = firstBasicBlock
    val inFrame = initialFrame
    val outFrame = basicBlock.interpret(inFrame)
    outFrame.cg

    /*
      val worklist = scala.collection.mutable.Set[(BasicBlock, Frame)]((firstBasicBlock, initialFrame))

      val outState = collection.mutable.Map(
        (for {block <- controlFlowGraph} yield (block, initialFrame)): _*
      )

      // we use an upper bound in case the out states don't converge (i.e. we never reach a fixed point)
      val upperBound = 10 * controlFlowGraph.size
      var iteration = 0

      while (worklist.nonEmpty && iteration < upperBound) {
        iteration += 1

        val entry @ (basicBlock, frame) = worklist.head
        worklist -= entry

        val cgBefore = outState(basicBlock).cg
        val outStates = basicBlock.predecessors.map(outState)
        val inState = outStates.map(_.cg).fold(ConnectionGraph.empty())(_ merge _)
        val inFrame = initialFrame.copy(cg = inState)
        val outFrame = basicBlock.instructions.foldLeft(inFrame)((frame, i) => i.interpret(frame))
        outState(basicBlock) = outFrame
        val cgAfter = outFrame.cg


        // out state changed => we have to recalculate the out states for each successor
        if (cgBefore != cgAfter) {
          worklist ++= basicBlock.successors
        }
      }
      initialFrame.cg*/
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




