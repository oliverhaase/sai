package scala

import org.scalatest.{FlatSpec, Matchers}
import sai.bytecode.Clazz

class ExceptionInfoTest extends FlatSpec with Matchers {

  val clazz = new Clazz("misc.BasicBlockExamples")
  val method = clazz.lookupMethod("multipleTryCatch").get
  val exceptionInfo = method.exceptionInfo

  "An ExceptionInfo" should "find target successors" in {

    // try catch example
    var lastTryInstruction = method.lookup(5)
    var firstCatchInstruction = method.lookup(11)
    exceptionInfo.findTargetSuccessors(lastTryInstruction) shouldEqual List(firstCatchInstruction)

    // try catch finally example
    lastTryInstruction = method.lookup(33)
    firstCatchInstruction = method.lookup(47)
    var firstFinallyInstruction = method.lookup(67)
    exceptionInfo.findTargetSuccessors(lastTryInstruction) shouldEqual List(firstCatchInstruction, firstFinallyInstruction)
    val lastCatchInstruction = method.lookup(53)
    exceptionInfo.findTargetSuccessors(lastCatchInstruction) shouldEqual List(firstFinallyInstruction)

    // try finally example
    lastTryInstruction = method.lookup(91)
    firstFinallyInstruction = method.lookup(105)
    exceptionInfo.findTargetSuccessors(lastTryInstruction) shouldEqual List(firstFinallyInstruction)
  }

  it should "check if an instruction is within a try block" in {
    val insideTryRange =
      for (pc <- List(0, 3, 5, 28, 31, 33, 86, 89, 91))
        yield method.lookup(pc)
    insideTryRange.foreach(instruction => exceptionInfo.isInTryRange(instruction) shouldBe true)

    val outsideTryRange = method.instructions.diff(insideTryRange)
    outsideTryRange.foreach(instruction => exceptionInfo.isInTryRange(instruction) shouldBe false)
  }

}
