package scala


import org.scalatest.Matchers
import org.scalatest.FlatSpec
import sai.bytecode.Clazz

class BasicBlockTest extends FlatSpec with Matchers {

  "A BasicBlock" should "have 1 block for a method without control flow" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("simple").get
    val singleBlock :: Nil = method.controlFlowGraph

    singleBlock.lineRange shouldBe (7 to 11)
    singleBlock.successors shouldBe empty
    singleBlock.predecessors shouldBe empty
  }

  it should "have 3 blocks for a method with an if-statement" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("ifStatement").get
    val entryBlock :: ifStatement :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (65 to 67)
    entryBlock.successors shouldEqual List(ifStatement, exitBlock)
    entryBlock.predecessors shouldBe empty

    ifStatement.lineRange shouldBe (68 to 68)
    ifStatement.successors shouldEqual List(exitBlock)
    ifStatement.predecessors shouldEqual List(entryBlock)

    exitBlock.lineRange shouldBe (70 to 71)
    exitBlock.successors shouldBe empty
    exitBlock.predecessors shouldEqual List(entryBlock, ifStatement)
  }

  it should "have 4 blocks for a method with an if-else-statement" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("ifElseStatement").get
    val entryBlock :: ifBlock :: elseBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (73 to 75)
    entryBlock.successors shouldEqual List(ifBlock, elseBlock)
    entryBlock.predecessors shouldBe empty

    ifBlock.lineRange shouldBe (76 to 76)
    ifBlock.successors shouldEqual List(exitBlock)
    ifBlock.predecessors shouldEqual List(entryBlock)

    elseBlock.lineRange shouldBe (78 to 78)
    elseBlock.successors shouldEqual List(exitBlock)
    elseBlock.predecessors shouldEqual List(entryBlock)

    exitBlock.lineRange shouldBe (80 to 81)
    exitBlock.successors shouldBe empty
    exitBlock.predecessors shouldEqual List(ifBlock, elseBlock)
  }

  it should "have 4 blocks for a method with a while-loop" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("whileLoop").get
    val entryBlock :: whileCondition :: whileBody :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (83 to 85)
    entryBlock.successors shouldEqual List(whileCondition)
    entryBlock.predecessors shouldBe empty

    whileCondition.lineRange shouldBe (86 to 86)
    whileCondition.successors shouldEqual List(whileBody, exitBlock)
    whileCondition.predecessors shouldEqual List(entryBlock, whileBody)

    whileBody.lineRange shouldBe (87 to 87)
    whileBody.successors shouldEqual List(whileCondition)
    whileBody.predecessors shouldEqual List(whileCondition)

    exitBlock.lineRange shouldBe (89 to 90)
    exitBlock.successors shouldBe empty
    exitBlock.predecessors shouldEqual List(whileCondition)
  }

  it should "have 6 blocks for a method with an if-else-statement nested within a while loop" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("whileIfElse").get
    val entryBlock :: whileCondition :: whileBegin :: ifBlock :: elseBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (13 to 16)
    entryBlock.successors shouldEqual List(whileCondition)
    entryBlock.predecessors shouldBe empty

    whileCondition.lineRange shouldBe (17 to 17)
    whileCondition.successors shouldBe List(whileBegin, exitBlock)
    whileCondition.predecessors shouldBe List(entryBlock, ifBlock, elseBlock)

    whileBegin.lineRange shouldBe (18 to 19)
    whileBegin.successors shouldEqual List(ifBlock, elseBlock)
    whileBegin.predecessors shouldEqual List(whileCondition)

    ifBlock.lineRange shouldBe (20 to 20)
    ifBlock.successors shouldEqual List(whileCondition)
    ifBlock.predecessors shouldEqual List(whileBegin)

    elseBlock.lineRange shouldBe (22 to 22)
    elseBlock.successors shouldEqual List(whileCondition)
    elseBlock.predecessors shouldEqual List(whileBegin)

    exitBlock.lineRange shouldBe (25 to 26)
    exitBlock.successors shouldBe empty
    exitBlock.predecessors shouldEqual List(whileCondition)
  }

  it should "have 4 blocks for a method with a try-finally construct" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryFinally").get
    val entryBlock :: tryBlock :: finallyBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (29 to 30)
    entryBlock.successors shouldEqual List(tryBlock)
    entryBlock.predecessors shouldBe empty

    tryBlock.lineRange shouldBe (32 to 32)
    tryBlock.successors shouldEqual List(finallyBlock)
    tryBlock.predecessors shouldEqual List(entryBlock)

    finallyBlock.lineRange shouldBe (34 to 35)
    finallyBlock.predecessors shouldEqual List(tryBlock)
    finallyBlock.successors shouldEqual List(exitBlock)

    exitBlock.lineRange shouldBe (36 to 37)
    exitBlock.predecessors shouldEqual List(finallyBlock)
    exitBlock.successors shouldBe empty
  }

  it should "have 5 blocks for a method with a try-catch-finally construct" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatchFinally").get
    val entryBlock :: tryBlock :: catchBlock :: finallyBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (39 to 40)
    entryBlock.successors shouldEqual List(tryBlock)
    entryBlock.predecessors shouldBe empty

    tryBlock.lineRange shouldBe (42 to 42)
    tryBlock.successors shouldEqual List(catchBlock, finallyBlock)
    tryBlock.predecessors shouldEqual List(entryBlock)

    catchBlock.lineRange shouldBe (43 to 44)
    catchBlock.successors shouldEqual List(finallyBlock)
    catchBlock.predecessors shouldEqual List(tryBlock)

    finallyBlock.lineRange shouldBe (46 to 47)
    finallyBlock.successors shouldEqual List(exitBlock)
    finallyBlock.predecessors shouldEqual List(tryBlock, catchBlock)

    exitBlock.lineRange shouldBe (48 to 49)
    exitBlock.successors shouldBe empty
    exitBlock.predecessors shouldEqual List(finallyBlock)
  }

  it should "have 6 blocks for a method with a try-catch-catch-finally construct" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatchCatchFinally").get
    val entryBlock :: tryBlock :: catchBlock1 :: catchBlock2 :: finallyBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (51 to 52)
    entryBlock.successors shouldEqual List(tryBlock)
    entryBlock.predecessors shouldBe empty

    tryBlock.lineRange shouldBe (54 to 54)
    tryBlock.successors shouldEqual List(catchBlock1, catchBlock2, finallyBlock)
    tryBlock.predecessors shouldEqual List(entryBlock)

    catchBlock1.lineRange shouldBe (55 to 56)
    catchBlock1.successors shouldEqual List(finallyBlock)
    catchBlock1.predecessors shouldEqual List(tryBlock)

    catchBlock2.lineRange shouldBe (57 to 58)
    catchBlock2.successors shouldEqual List(finallyBlock)
    catchBlock2.predecessors shouldEqual List(tryBlock)

    finallyBlock.lineRange shouldBe (60 to 61)
    finallyBlock.successors shouldEqual List(exitBlock)
    finallyBlock.predecessors shouldEqual List(tryBlock, catchBlock1, catchBlock2)

    exitBlock.lineRange shouldBe (62 to 63)
    exitBlock.successors shouldBe empty
    exitBlock.predecessors shouldEqual List(finallyBlock)
  }

  it should "have 4 blocks for a method with a try-catch construct" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatch").get
    val entryBlock :: tryBlock :: catchBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.lineRange shouldBe (92 to 93)
    entryBlock.predecessors shouldBe empty
    entryBlock.successors shouldEqual List(tryBlock)

    tryBlock.lineRange shouldBe (95 to 95)
    tryBlock.predecessors shouldEqual List(entryBlock)
    tryBlock.successors shouldEqual List(catchBlock, exitBlock)

    catchBlock.lineRange shouldBe (96 to 97)
    catchBlock.predecessors shouldEqual List(tryBlock)
    catchBlock.successors shouldEqual List(exitBlock)

    exitBlock.lineRange shouldBe (99 to 100)
    exitBlock.predecessors shouldEqual List(tryBlock, catchBlock)
    exitBlock.successors shouldBe empty
  }

  it should "have 1 block for a method without any instructions" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("emptyMethod").get
    val singleBlock :: Nil = method.controlFlowGraph

    singleBlock.lineRange shouldBe (102 to 103)
    singleBlock.predecessors shouldBe empty
    singleBlock.successors shouldBe empty
  }

}