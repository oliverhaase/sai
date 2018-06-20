package scala


import org.scalatest.Matchers
import org.scalatest.FlatSpec
import sai.bytecode.Clazz

class BasicBlockTest extends FlatSpec with Matchers {

  val clazz = new Clazz("misc.BasicBlockExamples")

  "A ControlFlowGraph" should "have 1 basic block for a method without control flow instructions" in {
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

  it should "have 4 basic blocks for a method with a try-finally construct" in {
    val method = clazz.method("tryFinally").get
    val entryTryBlock :: finallyBlock :: finallyThrowBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.successors shouldEqual List(finallyBlock, finallyThrowBlock)

    finallyBlock.predecessors shouldEqual List(entryTryBlock)
    finallyBlock.successors shouldEqual List(exitBlock)

    finallyThrowBlock.predecessors shouldEqual List(entryTryBlock)
    finallyThrowBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(finallyBlock, finallyThrowBlock)
    exitBlock.successors shouldBe empty
  }

  it should "have 7 basic blocks for a method with a try-catch-finally construct" in {
    val method = clazz.method("tryCatchFinally").get
    val entryTryBlock :: tryFinallyBlock :: catchBlock :: catchFinallyBlock :: finallyThrowBlock :: preExitBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.successors shouldEqual List(tryFinallyBlock, catchBlock, finallyThrowBlock)

    tryFinallyBlock.predecessors shouldEqual List(entryTryBlock)
    tryFinallyBlock.successors shouldEqual List(preExitBlock)

    catchBlock.predecessors shouldEqual List(entryTryBlock)
    catchBlock.successors shouldEqual List(catchFinallyBlock, finallyThrowBlock)

    catchFinallyBlock.predecessors shouldEqual List(catchBlock)
    catchFinallyBlock.successors shouldEqual List(preExitBlock)

    finallyThrowBlock.predecessors shouldEqual List(entryTryBlock, catchBlock)
    finallyThrowBlock.successors shouldEqual List(exitBlock)

    preExitBlock.predecessors shouldEqual List(tryFinallyBlock, catchFinallyBlock)
    preExitBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(finallyThrowBlock, preExitBlock)
    exitBlock.successors shouldBe empty
  }

  it should "have 9 basic blocks for a method with a try-catch-catch-finally construct" in {
    val method = clazz.method("tryCatchCatchFinally").get
    val entryTryBlock :: tryFinallyBlock :: catch1Block :: catch1FinallyBlock :: catch2Block :: catch2FinallyBlock :: finallyThrowBlock :: preExitBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.successors shouldEqual List(tryFinallyBlock, catch1Block, catch2Block, finallyThrowBlock)

    tryFinallyBlock.predecessors shouldEqual List(entryTryBlock)
    tryFinallyBlock.successors shouldEqual List(preExitBlock)

    catch1Block.predecessors shouldEqual List(entryTryBlock)
    catch1Block.successors shouldEqual List(catch1FinallyBlock, finallyThrowBlock)

    catch1FinallyBlock.predecessors shouldEqual List(catch1Block)
    catch1FinallyBlock.successors shouldEqual List(preExitBlock)

    catch2Block.predecessors shouldEqual List(entryTryBlock)
    catch2Block.successors shouldEqual List(catch2FinallyBlock, finallyThrowBlock)

    catch2FinallyBlock.predecessors shouldEqual List(catch2Block)
    catch2FinallyBlock.successors shouldEqual List(preExitBlock)

    finallyThrowBlock.predecessors shouldEqual List(entryTryBlock, catch1Block, catch2Block)
    finallyThrowBlock.successors shouldEqual List(exitBlock)

    preExitBlock.predecessors shouldEqual List(tryFinallyBlock, catch1FinallyBlock, catch2FinallyBlock)
    preExitBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(finallyThrowBlock, preExitBlock)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 basic blocks for a method with a try-catch construct" in {
    val method = clazz.method("tryCatch").get
    val entryTryBlock :: gotoBlock :: catchBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryTryBlock.predecessors shouldBe empty
    entryTryBlock.successors shouldEqual List(gotoBlock, catchBlock)

    gotoBlock.predecessors shouldEqual List(entryTryBlock)
    gotoBlock.successors shouldEqual List(exitBlock)

    catchBlock.predecessors shouldEqual List(entryTryBlock)
    catchBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(gotoBlock, catchBlock)
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

  it should "have 3 basic blocks for a method with a do-while loop" in {
    val method = clazz.method("doWhile").get
    val entryBlock :: doWhileLoop :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (148 to 149)
    entryBlock.successors shouldEqual List(doWhileLoop)

    doWhileLoop.predecessors shouldEqual List(entryBlock, doWhileLoop)
    doWhileLoop.lineRange shouldBe (151 to 152)
    doWhileLoop.successors shouldEqual List(doWhileLoop, exitBlock)

    exitBlock.predecessors shouldEqual List(doWhileLoop)
    exitBlock.lineRange shouldBe (153 to 154)
    exitBlock.successors shouldBe empty
  }

  it should "have 4 basic blocks for a method with a foor loop" in {
    val method = clazz.method("foorLoop").get
    val q = method.controlFlowGraph
    val entryBlock :: loopCondition :: loopBody :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (156 to 158)
    entryBlock.successors shouldEqual List(loopCondition)

    loopCondition.predecessors shouldEqual List(entryBlock, loopBody)
    loopCondition.lineRange shouldBe (158 to 158)
    loopCondition.successors shouldEqual List(loopBody, exitBlock)

    loopBody.predecessors shouldBe List(loopCondition)
    loopBody.lineRange shouldBe (159 to 158)
    loopBody.successors shouldEqual List(loopCondition)

    exitBlock.predecessors shouldEqual List(loopCondition)
    exitBlock.lineRange shouldBe (161 to 162)
    exitBlock.successors shouldBe empty
  }

  it should "have 5 basic blocks for a method with a switch-case-case-default statement" in {
    val method = clazz.method("switchCaseCaseDefault").get
    val entryBlock :: case1Block :: case2Block :: defaultBlock :: exitBlock :: Nil = method.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.lineRange shouldBe (164 to 167)
    entryBlock.successors shouldEqual List(case1Block, case2Block, defaultBlock)

    case1Block.predecessors shouldEqual List(entryBlock)
    case1Block.lineRange shouldBe (169 to 170)
    case1Block.successors shouldEqual List(exitBlock)

    case2Block.predecessors shouldEqual List(entryBlock)
    case2Block.lineRange shouldBe (172 to 173)
    case2Block.successors shouldEqual List(exitBlock)

    defaultBlock.predecessors shouldEqual List(entryBlock)
    defaultBlock.lineRange shouldBe (175 to 175)
    defaultBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(case1Block, case2Block, defaultBlock)
    exitBlock.lineRange shouldBe (178 to 179)
    exitBlock.successors shouldBe empty
  }

  it should "define 6 basic blocks for function 'isPalindrome'" in {
    val palindromeChecker = new Clazz("misc.PalindromeChecker")
    val isPalindrome = palindromeChecker.method("isPalindrome").get
    val entryBlock :: whileCheck :: ifCheck :: thenBlock :: elseBlock :: exitBlock :: Nil = isPalindrome.controlFlowGraph

    entryBlock.predecessors shouldBe empty
    entryBlock.successors shouldEqual List(whileCheck)

    whileCheck.predecessors shouldEqual List(entryBlock, thenBlock)
    whileCheck.successors shouldEqual List(ifCheck, exitBlock)

    ifCheck.predecessors shouldEqual List(whileCheck)
    ifCheck.successors shouldEqual List(thenBlock, elseBlock)

    thenBlock.predecessors shouldEqual List(ifCheck)
    thenBlock.successors shouldEqual List(whileCheck)

    elseBlock.predecessors shouldEqual List(ifCheck)
    elseBlock.successors shouldEqual List(exitBlock)

    exitBlock.predecessors shouldEqual List(whileCheck, elseBlock)
    exitBlock.successors shouldBe empty
  }

}