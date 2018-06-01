package scala


import org.scalatest.Matchers
import org.scalatest.FlatSpec
import sai.bytecode.Clazz

class BasicBlockTest extends FlatSpec with Matchers {

  val clazz = new Clazz("misc.BasicBlockExamples")

  "A ControlFlowGraph" should "have 1 basic block for a method without control flow" in {
    val method = clazz.method("simple").get
    val singleBlock :: Nil = method.controlFlowGraph

    singleBlock.predecessors shouldBe empty
    singleBlock.lineRange shouldBe (7 to 11)
    singleBlock.successors shouldBe empty
  }

  it should "have 3 basic blocks for a method with an if-statement" in {
    val method = clazz.method("ifStatement").get
    val entryBlock :: ifBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (65 to 67)
    entryBlock.successors shouldEqual List(ifBlock, exitBlock)

    ifBlock.predecessors shouldEqual List(entryBlock)
    ifBlock.lineRange shouldBe (68 to 68)
    ifBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(entryBlock, ifBlock)
    exitBlock.lineRange shouldBe (70 to 71)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 basic blocks for a method with an if-else-statement" in {
    val method = clazz.method("ifElseStatement").get
    val entryBlock :: ifBlock :: elseBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (73 to 75)
    entryBlock.successors shouldEqual List(ifBlock, elseBlock)

    ifBlock.predecessors shouldEqual List(entryBlock)
    ifBlock.lineRange shouldBe (76 to 76)
    ifBlock.successors shouldEqual List(exitBlock)

    elseBlock.predecessors shouldEqual List(entryBlock)
    elseBlock.lineRange shouldBe (78 to 78)
    elseBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(ifBlock, elseBlock)
    exitBlock.lineRange shouldBe (80 to 81)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 basic blocks for a method with a while-loop" in {
    val method = clazz.method("whileLoop").get
    val entryBlock :: whileCondition :: whileBody :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (83 to 85)
    entryBlock.successors shouldEqual List(whileCondition)

    whileCondition.predecessors shouldEqual List(entryBlock, whileBody)
    whileCondition.lineRange shouldBe (86 to 86)
    whileCondition.successors shouldEqual List(whileBody, exitBlock)

    whileBody.predecessors shouldEqual List(whileCondition)
    whileBody.lineRange shouldBe (87 to 87)
    whileBody.successors shouldEqual List(whileCondition)

    exitBlock.predecessors shouldEqual List(whileCondition)
    exitBlock.lineRange shouldBe (89 to 90)
    exitBlock.successors shouldBe empty
  }

  it should "have 6 basic blocks for a method with an if-else-statement nested within a while loop" in {
    val method = clazz.method("whileIfElse").get
    val entryBlock :: whileCondition :: whileBegin :: ifBlock :: elseBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (13 to 16)
    entryBlock.successors shouldEqual List(whileCondition)

    whileCondition.predecessors shouldBe List(entryBlock, ifBlock, elseBlock)
    whileCondition.lineRange shouldBe (17 to 17)
    whileCondition.successors shouldBe List(whileBegin, exitBlock)

    whileBegin.predecessors shouldEqual List(whileCondition)
    whileBegin.lineRange shouldBe (18 to 19)
    whileBegin.successors shouldEqual List(ifBlock, elseBlock)

    ifBlock.predecessors shouldEqual List(whileBegin)
    ifBlock.lineRange shouldBe (20 to 20)
    ifBlock.successors shouldEqual List(whileCondition)

    elseBlock.predecessors shouldEqual List(whileBegin)
    elseBlock.lineRange shouldBe (22 to 22)
    elseBlock.successors shouldEqual List(whileCondition)

    exitBlock.predecessors shouldEqual List(whileCondition)
    exitBlock.lineRange shouldBe (25 to 26)
    exitBlock.successors shouldBe empty
  }

  it should "have 1 basic block for a method with a try-finally construct" in {
    val method = clazz.method("tryFinally").get
    val block :: Nil = method.controlFlowGraph

    block.predecessors shouldBe empty
    block.lineRange shouldBe (29 to 37)
    block.successors shouldBe empty
  }

  it should "have 3 basic blocks for a method with a try-catch-finally construct" in {
    val method = clazz.method("tryCatchFinally").get
    val entryTryBlock :: catchBlock :: finallyExitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (39 to 42)
    entryTryBlock.successors shouldEqual List(catchBlock, finallyExitBlock)

    catchBlock.predecessors shouldEqual List(entryTryBlock)
    catchBlock.lineRange shouldBe (43 to 44)
    catchBlock.successors shouldEqual List(finallyExitBlock)

    finallyExitBlock.predecessors shouldEqual List(entryTryBlock, catchBlock)
    finallyExitBlock.lineRange shouldBe (46 to 49)
    finallyExitBlock.successors shouldBe empty
  }

  it should "have 4 basic blocks for a method with a try-catch-catch-finally construct" in {
    val method = clazz.method("tryCatchCatchFinally").get
    val entryTryBlock :: catchBlock1 :: catchBlock2 :: finallyExitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (51 to 54)
    entryTryBlock.successors shouldEqual List(catchBlock1, catchBlock2, finallyExitBlock)

    catchBlock1.predecessors shouldEqual List(entryTryBlock)
    catchBlock1.lineRange shouldBe (55 to 56)
    catchBlock1.successors shouldEqual List(finallyExitBlock)

    catchBlock2.predecessors shouldEqual List(entryTryBlock)
    catchBlock2.lineRange shouldBe (57 to 58)
    catchBlock2.successors shouldEqual List(finallyExitBlock)

    finallyExitBlock.predecessors shouldEqual List(entryTryBlock, catchBlock1, catchBlock2)
    finallyExitBlock.lineRange shouldBe (60 to 63)
    finallyExitBlock.successors shouldBe empty
  }

  it should "have 3 basic blocks for a method with a try-catch construct" in {
    val method = clazz.method("tryCatch").get
    val entryTryBlock :: catchBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.lineRange shouldBe (92 to 95)
    entryTryBlock.successors shouldEqual List(catchBlock, exitBlock)

    catchBlock.predecessors shouldEqual List(entryTryBlock)
    catchBlock.lineRange shouldBe (96 to 97)
    catchBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(entryTryBlock, catchBlock)
    exitBlock.lineRange shouldBe (99 to 100)
    exitBlock.successors shouldBe empty
  }

  it should "have 1 basic block for a method without any instructions" in {
    val method = clazz.method("emptyMethod").get
    val singleBlock :: Nil = method.controlFlowGraph

    singleBlock.predecessors shouldBe empty
    singleBlock.lineRange shouldBe (102 to 103)
    singleBlock.successors shouldBe empty
  }

  it should "have 4 basic block for a method without any instructions" in {
    val method = clazz.method("multipleReturns").get
    val entryBlock :: ifBlock :: afterIfBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (105 to 107)
    entryBlock.successors shouldEqual List(ifBlock, afterIfBlock)

    ifBlock.predecessors shouldEqual List(entryBlock)
    ifBlock.lineRange shouldBe (108 to 109)
    ifBlock.successors shouldEqual List(exitBlock)

    afterIfBlock.predecessors shouldEqual List(entryBlock)
    afterIfBlock.lineRange shouldBe (111 to 112)
    afterIfBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(ifBlock, afterIfBlock)
    exitBlock.lineRange shouldBe (112 to 112)
    exitBlock.successors shouldBe empty
  }

}