package scala


import org.scalatest.Matchers
import org.scalatest.FlatSpec
import sai.bytecode.Clazz

class BasicBlockTest extends FlatSpec with Matchers {

  val clazz = new Clazz("misc.BasicBlockExamples")

  "A ControlFlowGraph" should "have 2 blocks for a method without control flow" in {
    val method = clazz.method("simple").get
    val entryBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (7 to 11)
    entryBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(entryBlock)
    exitBlock.lineRange shouldBe (11 to 11)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 blocks for a method with an if-statement" in {
    val method = clazz.method("ifStatement").get
    val entryBlock :: ifBlock :: afterIfBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (65 to 67)
    entryBlock.successors shouldEqual List(ifBlock, afterIfBlock)

    ifBlock.predecessors shouldEqual List(entryBlock)
    ifBlock.lineRange shouldBe (68 to 68)
    ifBlock.successors shouldEqual List(afterIfBlock)

    afterIfBlock.predecessors shouldEqual List(entryBlock, ifBlock)
    afterIfBlock.lineRange shouldBe (70 to 71)
    afterIfBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterIfBlock)
    exitBlock.lineRange shouldBe (71 to 71)
    exitBlock.successors shouldBe empty
  }

  it should "have 5 blocks for a method with an if-else-statement" in {
    val method = clazz.method("ifElseStatement").get
    val entryBlock :: ifBlock :: elseBlock :: afterIfElseBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (73 to 75)
    entryBlock.successors shouldEqual List(ifBlock, elseBlock)

    ifBlock.predecessors shouldEqual List(entryBlock)
    ifBlock.lineRange shouldBe (76 to 76)
    ifBlock.successors shouldEqual List(afterIfElseBlock)

    elseBlock.predecessors shouldEqual List(entryBlock)
    elseBlock.lineRange shouldBe (78 to 78)
    elseBlock.successors shouldEqual List(afterIfElseBlock)

    afterIfElseBlock.predecessors shouldEqual List(ifBlock, elseBlock)
    afterIfElseBlock.lineRange shouldBe (80 to 81)
    afterIfElseBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterIfElseBlock)
    exitBlock.lineRange shouldBe (81 to 81)
    exitBlock.successors shouldBe empty
  }

  it should "have 5 blocks for a method with a while-loop" in {
    val method = clazz.method("whileLoop").get
    val entryBlock :: whileCondition :: whileBody :: afterWhileBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (83 to 85)
    entryBlock.successors shouldEqual List(whileCondition)

    whileCondition.predecessors shouldEqual List(entryBlock, whileBody)
    whileCondition.lineRange shouldBe (86 to 86)
    whileCondition.successors shouldEqual List(whileBody, afterWhileBlock)

    whileBody.predecessors shouldEqual List(whileCondition)
    whileBody.lineRange shouldBe (87 to 87)
    whileBody.successors shouldEqual List(whileCondition)

    afterWhileBlock.predecessors shouldEqual List(whileCondition)
    afterWhileBlock.lineRange shouldBe (89 to 90)
    afterWhileBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterWhileBlock)
    exitBlock.lineRange shouldBe (90 to 90)
    exitBlock.successors shouldBe empty
  }

  it should "have 7 blocks for a method with an if-else-statement nested within a while loop" in {
    val method = clazz.method("whileIfElse").get
    val entryBlock :: whileCondition :: whileBegin :: ifBlock :: elseBlock :: afterWhileBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (13 to 16)
    entryBlock.successors shouldEqual List(whileCondition)

    whileCondition.predecessors shouldBe List(entryBlock, ifBlock, elseBlock)
    whileCondition.lineRange shouldBe (17 to 17)
    whileCondition.successors shouldBe List(whileBegin, afterWhileBlock)

    whileBegin.predecessors shouldEqual List(whileCondition)
    whileBegin.lineRange shouldBe (18 to 19)
    whileBegin.successors shouldEqual List(ifBlock, elseBlock)

    ifBlock.predecessors shouldEqual List(whileBegin)
    ifBlock.lineRange shouldBe (20 to 20)
    ifBlock.successors shouldEqual List(whileCondition)

    elseBlock.predecessors shouldEqual List(whileBegin)
    elseBlock.lineRange shouldBe (22 to 22)
    elseBlock.successors shouldEqual List(whileCondition)

    afterWhileBlock.predecessors shouldEqual List(whileCondition)
    afterWhileBlock.lineRange shouldBe (25 to 26)
    afterWhileBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterWhileBlock)
    exitBlock.lineRange shouldBe (26 to 26)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 blocks for a method with a try-finally construct" in {
    val method = clazz.method("tryFinally").get
    val entryTryBlock :: finallyBlock :: afterFinallyBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (29 to 32)
    entryTryBlock.successors shouldEqual List(finallyBlock)

    finallyBlock.predecessors shouldEqual List(entryTryBlock)
    finallyBlock.lineRange shouldBe (34 to 35)
    finallyBlock.successors shouldEqual List(afterFinallyBlock)

    afterFinallyBlock.predecessors shouldEqual List(finallyBlock)
    afterFinallyBlock.lineRange shouldBe (36 to 37)
    afterFinallyBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterFinallyBlock)
    exitBlock.lineRange shouldBe (37 to 37)
    exitBlock.successors shouldBe empty
  }

  it should "have 5 blocks for a method with a try-catch-finally construct" in {
    val method = clazz.method("tryCatchFinally").get
    val entryTryBlock :: catchBlock :: finallyBlock :: afterFinallyBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (39 to 42)
    entryTryBlock.successors shouldEqual List(catchBlock, finallyBlock)

    catchBlock.predecessors shouldEqual List(entryTryBlock)
    catchBlock.lineRange shouldBe (43 to 44)
    catchBlock.successors shouldEqual List(finallyBlock)

    finallyBlock.predecessors shouldEqual List(entryTryBlock, catchBlock)
    finallyBlock.lineRange shouldBe (46 to 47)
    finallyBlock.successors shouldEqual List(afterFinallyBlock)

    afterFinallyBlock.predecessors shouldEqual List(finallyBlock)
    afterFinallyBlock.lineRange shouldBe (48 to 49)
    afterFinallyBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterFinallyBlock)
    exitBlock.lineRange shouldBe (49 to 49)
    exitBlock.successors shouldBe empty
  }

  it should "have 6 blocks for a method with a try-catch-catch-finally construct" in {
    val method = clazz.method("tryCatchCatchFinally").get
    val entryTryBlock :: catchBlock1 :: catchBlock2 :: finallyBlock :: afterFinallyBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (51 to 54)
    entryTryBlock.successors shouldEqual List(catchBlock1, catchBlock2, finallyBlock)

    catchBlock1.predecessors shouldEqual List(entryTryBlock)
    catchBlock1.lineRange shouldBe (55 to 56)
    catchBlock1.successors shouldEqual List(finallyBlock)

    catchBlock2.predecessors shouldEqual List(entryTryBlock)
    catchBlock2.lineRange shouldBe (57 to 58)
    catchBlock2.successors shouldEqual List(finallyBlock)

    finallyBlock.predecessors shouldEqual List(entryTryBlock, catchBlock1, catchBlock2)
    finallyBlock.lineRange shouldBe (60 to 61)
    finallyBlock.successors shouldEqual List(afterFinallyBlock)

    afterFinallyBlock.predecessors shouldEqual List(finallyBlock)
    afterFinallyBlock.lineRange shouldBe (62 to 63)
    afterFinallyBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterFinallyBlock)
    exitBlock.lineRange shouldBe (63 to 63)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 blocks for a method with a try-catch construct" in {
    val method = clazz.method("tryCatch").get
    val entryTryBlock :: catchBlock :: afterCatchBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (92 to 95)
    entryTryBlock.successors shouldEqual List(catchBlock, afterCatchBlock)

    catchBlock.predecessors shouldEqual List(entryTryBlock)
    catchBlock.lineRange shouldBe (96 to 97)
    catchBlock.successors shouldEqual List(afterCatchBlock)

    afterCatchBlock.predecessors shouldEqual List(entryTryBlock, catchBlock)
    afterCatchBlock.lineRange shouldBe (99 to 100)
    afterCatchBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(afterCatchBlock)
    exitBlock.lineRange shouldBe (100 to 100)
    exitBlock.successors shouldBe empty
  }

  it should "have 2 blocks for a method without any instructions" in {
    val method = clazz.method("emptyMethod").get
    val entryBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (102 to 103)
    entryBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(entryBlock)
    exitBlock.lineRange shouldBe (103 to 103)
    exitBlock.successors shouldBe empty
  }

}