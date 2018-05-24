import org.scalatest.FlatSpec
import org.scalatest.Matchers
import sai.bytecode.Clazz

class MethodTest extends FlatSpec with Matchers {

  "A method" should "have 1 basic block when using no control flow instruction" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("simple").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 1
    basicBlocks(0).startLine shouldBe 7
  }

  it should "have 3 basic blocks for using an if-statement" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("ifStatement").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 3
    basicBlocks(0).startLine shouldBe 65
    basicBlocks(1).startLine shouldBe 68
    basicBlocks(2).startLine shouldBe 70
  }

  it should "have 4 basic blocks for using a if-else statement" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("ifElseStatement").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 4
    basicBlocks(0).startLine shouldBe 73
    basicBlocks(1).startLine shouldBe 76
    basicBlocks(2).startLine shouldBe 78
    basicBlocks(3).startLine shouldBe 80
  }

  it should "have 4 basic blocks for using a while loop" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("whileLoop").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 4
    basicBlocks(0).startLine shouldBe 83
    basicBlocks(1).startLine shouldBe 86
    basicBlocks(2).startLine shouldBe 87
    basicBlocks(3).startLine shouldBe 89
  }

  it should "have 6 basic blocks for nested if-else within a while-loop" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("whileIfElse").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 6
    basicBlocks(0).startLine shouldBe 13
    basicBlocks(1).startLine shouldBe 17
    basicBlocks(2).startLine shouldBe 18
    basicBlocks(3).startLine shouldBe 20
    basicBlocks(4).startLine shouldBe 22
    basicBlocks(5).startLine shouldBe 25
  }

  it should "have 3 basic blocks for using a try-finally method" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryFinally").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 3
    basicBlocks(0).startLine shouldBe 29
    basicBlocks(1).startLine shouldBe 34
    basicBlocks(2).startLine shouldBe 36
  }

  it should "have 4 basic blocks when for using a try-catch-finally construct" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatchFinally").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 4
    basicBlocks(0).startLine shouldBe 39
    basicBlocks(1).startLine shouldBe 43
    basicBlocks(2).startLine shouldBe 46
    basicBlocks(3).startLine shouldBe 48
  }

  it should "have 5 basic blocks for using a try-catch-catch-finally construct" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatchCatchFinally").get
    val basicBlocks = method.basicBlocks
    basicBlocks.size shouldBe 5
    basicBlocks(0).startLine shouldBe 51
    basicBlocks(1).startLine shouldBe 55
    basicBlocks(2).startLine shouldBe 57
    basicBlocks(3).startLine shouldBe 60
    basicBlocks(4).startLine shouldBe 62
  }

}
