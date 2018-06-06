package bytecode

import scala.annotation.tailrec

import org.apache.bcel.classfile.CodeException
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.ControlFlowInstruction

object ExceptionInfo {

  def apply(method: Method, codeExceptions: List[CodeException]): ExceptionInfo = {
    val exceptionBlocks = findExceptionBlocks(method, codeExceptions, Nil)
    new ExceptionInfo(exceptionBlocks)
  }

  @tailrec
  private[this] def findExceptionBlocks(m: Method, exceptions: List[CodeException], exceptionBlocks: List[ExceptionBlock]): List[ExceptionBlock] = exceptions match {
    case Nil =>
      exceptionBlocks
    case exception :: _ =>
      val tryRange = exception.getStartPC to exception.getEndPC
      val catchBlocks = exceptions.filter(e => e.getCatchType != 0 && e.getStartPC == tryRange.start)
      val finallyBlock = exceptions.find(e => e.getCatchType == 0 && e.getStartPC == tryRange.start)

      // there is a finally handler in the exception table for each catch handler -> drop 1 + size of catch blocks
      val toDrop = catchBlocks.size + finallyBlock.fold(0)(_ => 1 + catchBlocks.size)
      val tail = exceptions.drop(toDrop)
      findExceptionBlocks(m, tail, exceptionBlocks :+ new ExceptionBlock(m, tryRange, catchBlocks, finallyBlock))
  }
}

class ExceptionInfo(exceptionBlocks: List[ExceptionBlock]) {

  def isFirstTryInstruction(instruction: Instruction) =
    exceptionBlocks.exists(_.firstTryInstruction == instruction)

  def isInTryRange(instruction: Instruction) =
    exceptionBlocks.exists(_.isInTryRange(instruction))

  def isCatchLeader(instruction: Instruction) =
    exceptionBlocks.exists(_.catchHandlers.contains(instruction))

  def findCatchHandlers(gotoInstruction: ControlFlowInstruction) = {
    val isHandlerPredecessor = exceptionBlocks.exists(_.handlerPredecessor == gotoInstruction)
    if (isHandlerPredecessor) {
      exceptionBlocks.find(_.handlerPredecessor == gotoInstruction).map(_.catchHandlers).getOrElse(Nil)
    } else {
      Nil
    }
  }

  def isFinallyLeader(instruction: Instruction) =
    exceptionBlocks.exists(_.finallyHandler.contains(instruction))

  def isWithinFinallyBlock(instruction: Instruction) =
    exceptionBlocks.exists(_.isWithinFinallyBlock(instruction))

}

private class ExceptionBlock(method: Method, tryRange: Range, catchBlocks: List[CodeException], finallyBlock: Option[CodeException]) {

  def firstTryInstruction = method.lookup(tryRange.start)

  def lastTryInstruction = method.lookup(tryRange.end).prev

  def isInTryRange(instruction: Instruction) = instruction.pc match {
    case Some(pc) =>
      val range = firstTryInstruction.pc.get to lastTryInstruction.pc.get
      range.contains(pc)
    case None =>
      false
  }

  def handlerPredecessor: Instruction = handlers(0).prev

  def successor = handlers(0).prev.successors.max

  def handlers = catchHandlers ::: finallyHandler.toList

  def catchHandlers =
    for (catchBlock <- catchBlocks)
      yield method.lookup(catchBlock.getHandlerPC)

  def finallyHandler =
    for (fb <- finallyBlock)
      yield method.lookup(fb.getHandlerPC)

  def isWithinFinallyBlock(instruction: Instruction) = (instruction.pc, finallyBlock) match {
    case (Some(pc), Some(fb)) if pc >= fb.getHandlerPC =>
      val finallyRange = fb.getHandlerPC to successor.pc.get
      finallyRange.contains(pc)
    case _ =>
      false
  }

}