package bytecode

import org.apache.bcel.classfile.CodeException
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction
import sai.bytecode.instruction.ControlFlowInstruction

class ExceptionInfo(method: Method, codeExceptions: Array[CodeException]) {

  private def exceptionHandlers =
    for (codeException <- codeExceptions.toList)
      yield new ExceptionHandler(method, codeException)

  /**
   * The java compiler copies all finally statements to the end of the try block
   * and to the end of each catch block (since 'jsr' instruction is deprecated).
   *
   * The original statements of the finally block however are still compiled to the bytecode.
   * These instructions are inserted after the try block or after the last catch block.
   * The finally-block ends with an 'athrow' instruction which can never be thrown
   * since the code in this block will never be executed.
   *
   * @param throwInstruction instruction to check
   * @return true if the given throw instruction is the last statement in a finally block, false otherwise.
   */
  def isThrowInFinallyBlock(throwInstruction: Instruction) = {
    val handlers =
      for (exceptionHandler <- exceptionHandlers if exceptionHandler.catchesAnyException)
        yield exceptionHandler
    handlers.exists { handler =>
      val targetInstruction = method.lookup(handler.target)
      val prev = targetInstruction.prev
      prev match {
        case i: ControlFlowInstruction if i.isGoto =>
          val gotoTarget = i.successors.head
          gotoTarget.prev == throwInstruction
        case _ =>
            false
      }
    }
  }

  def isTryLeader(instruction: Instruction) =
    exceptionHandlers.exists(_.firstTryInstruction == instruction)

  def isLastTryInstruction(instruction: Instruction) =
    exceptionHandlers.exists(_.lastTryInstruction == instruction)

  def isCatchLeader(instruction: Instruction) =
    exceptionHandlers.exists(_.firstCatchInstruction == instruction)

  def isInsideTryBlock(instruction: Instruction) =
    exceptionHandlers.exists(_.handles(instruction))

  def findCatchLeaders(instruction: Instruction) = {
    assert(isLastTryInstruction(instruction))

    val catchLeaders =
      for (exceptionHandler <- exceptionHandlers if exceptionHandler.handles(instruction))
        yield exceptionHandler.firstCatchInstruction

    // check the successor of the last try instruction
    instruction.next match {
      case x: ControlFlowInstruction =>
        // it is a branch instruction, so there is no finally block!
        // -> add the targets of the branch instruction to the leader list
        x.successors ::: catchLeaders
      case _ =>
        // it is not a branch instruction, so there is a finally block!
        // -> the finally leader is already also in the catch leaders list
        catchLeaders
    }
  }

}

private class ExceptionHandler(method: Method, exception: CodeException) {

  def catchesAnyException: Boolean = exception.getCatchType == 0

  def target = exception.getHandlerPC

  def range = exception.getStartPC until exception.getEndPC

  def firstTryInstruction = method.lookup(exception.getStartPC)

  def lastTryInstruction = method.lookup { i =>
    i.pc.fold(false)(_ + i.length == exception.getEndPC)
  }

  def firstCatchInstruction = method.lookup(exception.getHandlerPC)

  def handles(instruction: Instruction) = instruction.pc.fold(false)(range.contains)

}