package bytecode

import org.apache.bcel.classfile.CodeException
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction

class ExceptionHandlerInfo(method: Method, codeExceptions: Array[CodeException]) {

  private def exceptionHandlers =
    for (codeException <- codeExceptions.toList)
      yield new ExceptionHandler(method, codeException)

  def isTryLeader(instruction: Instruction) =
    exceptionHandlers.exists(_.firstTryInstruction == instruction)

  def isLastTryInstruction(instruction: Instruction) =
    exceptionHandlers.exists(_.lastTryInstruction == instruction)

  def isCatchLeader(instruction: Instruction) =
    exceptionHandlers.exists(_.firstCatchInstruction == instruction)

  def isInsideTryBlock(instruction: Instruction) =
    exceptionHandlers.exists(_.handles(instruction))

  def findCatchLeaders(instruction: Instruction) =
    for (exceptionHandler <- exceptionHandlers if exceptionHandler.handles(instruction))
      yield exceptionHandler.firstCatchInstruction
}

private class ExceptionHandler(method: Method, exception: CodeException) {

  def range = exception.getStartPC until exception.getEndPC

  def firstTryInstruction = method.lookup(exception.getStartPC)

  def lastTryInstruction = method.lookup { i =>
    i.pc.fold(false)(_ + i.length == exception.getEndPC)
  }

  def firstCatchInstruction = method.lookup(exception.getHandlerPC)

  def handles(instruction: Instruction) = instruction.pc.fold(false)(range.contains)

}