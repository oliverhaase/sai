package bytecode

import org.apache.bcel.classfile.CodeException
import sai.bytecode.Method
import sai.bytecode.instruction.Instruction

object ExceptionInfo {
  def apply(method: Method, exceptionTable: List[CodeException]): ExceptionInfo = new ExceptionInfo(method, exceptionTable)
}

class ExceptionInfo(method: Method, exceptionTable: List[CodeException]) {

  private[this] implicit class CodeExceptionExtensions(codeException: CodeException) {
    private val any = 0
    def from = codeException.getStartPC
    def to = codeException.getEndPC
    def target = codeException.getHandlerPC
    def isFinally = codeException.getCatchType == any
    def isCatch = !isFinally
  }

  def findTargetSuccessors(instruction: Instruction) = {
    if (instruction.next == null)
      Nil
    else instruction.next.pc match {
      case Some(pc) =>
        for (entry <- exceptionTable if entry.to == pc)
          yield method.lookup(entry.target)
      case None => Nil
    }
  }

  def isInTryRange(instruction: Instruction) = {
    instruction.pc.fold(false)(pc => tryRanges.exists(_.contains(pc)))
  }

  private def tryRanges = {
    val targets = exceptionTable.map(_.target)

    val tryRanges =
      exceptionTable
      .filter(e => e.isCatch || !targets.contains(e.from)) // drop catch-finally entries
      .groupBy(e => e.from)
      .values
      .map(e => e.minBy(_.target)) // exception table does not guarantee any ordering
      .map(e => e.from until e.to)
    tryRanges
  }

}
