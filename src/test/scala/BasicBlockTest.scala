package scala


import org.scalatest.Matchers
import org.scalatest.FlatSpec
import sai.bytecode.Clazz

class BasicBlockTest extends FlatSpec with Matchers {

  "A BasicBlock" should "have no successors and no predecessors" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("simple").get
    val block1 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(7, 11)
    block1.successors shouldBe empty
    block1.predecessors shouldBe empty
  }

  it should "have successors and predecessors in ifStatement" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("ifStatement").get
    val block1 :: block2 :: block3 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(65, 67)
    block1.successors shouldEqual List(block2, block3)
    block1.predecessors shouldBe empty

    block2.lineRange shouldBe Range.inclusive(68, 68)
    block2.successors shouldEqual List(block3)
    block2.predecessors shouldEqual List(block1)

    block3.lineRange shouldBe Range.inclusive(70, 71)
    block3.successors shouldBe empty
    block3.predecessors shouldEqual List(block1, block2)
  }

  it should "have successors and predecessors in ifElseStatement" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("ifElseStatement").get
    val block1 :: block2 :: block3 :: block4 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(73, 75)
    block1.successors shouldEqual List(block2, block3)
    block1.predecessors shouldBe empty

    block2.lineRange shouldBe Range.inclusive(76, 76)
    block2.successors shouldEqual List(block4)
    block2.predecessors shouldEqual List(block1)

    block3.lineRange shouldBe Range.inclusive(78, 78)
    block3.successors shouldEqual List(block4)
    block3.predecessors shouldEqual List(block1)

    block4.lineRange shouldBe Range.inclusive(80, 81)
    block4.successors shouldBe empty
    block4.predecessors shouldEqual List(block2, block3)
  }

  it should "have successors and predecessors in whileLoop" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("whileLoop").get
    val block1 :: block2 :: block3 :: block4 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(83, 85)
    block1.successors shouldEqual List(block2)
    block1.predecessors shouldBe empty

    block2.lineRange shouldBe Range.inclusive(86, 86)
    block2.successors shouldEqual List(block3, block4)
    block2.predecessors shouldEqual List(block1, block3)

    block3.lineRange shouldBe Range.inclusive(87, 87)
    block3.successors shouldEqual List(block2)
    block3.predecessors shouldEqual List(block2)

    block4.lineRange shouldBe Range.inclusive(89, 90)
    block4.successors shouldBe empty
    block4.predecessors shouldEqual List(block2)
  }

  it should "have successors and predecessors in whileIfElse" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("whileIfElse").get
    val block1 :: block2 :: block3 :: block4 :: block5 :: block6 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(13, 16)
    block1.successors shouldEqual List(block2)
    block1.predecessors shouldBe empty

    block2.lineRange shouldBe Range.inclusive(17, 17)
    block2.successors shouldBe List(block3, block6)
    block2.predecessors shouldBe List(block1, block4, block5)

    block3.lineRange shouldBe Range.inclusive(18, 19)
    block3.successors shouldEqual List(block4, block5)
    block3.predecessors shouldEqual List(block2)

    block4.lineRange shouldBe Range.inclusive(20, 20)
    block4.successors shouldEqual List(block2)
    block4.predecessors shouldEqual List(block3)

    block5.lineRange shouldBe Range.inclusive(22, 22)
    block5.successors shouldEqual List(block2)
    block5.predecessors shouldEqual List(block3)

    block6.lineRange shouldBe Range.inclusive(25, 26)
    block6.successors shouldBe empty
    block6.predecessors shouldEqual List(block2)
  }

  it should "have successors and predecessors in tryFinally" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryFinally").get
    val block1 :: block2 :: block3 :: block4 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(29, 30)
    block1.successors shouldEqual List(block2)
    block1.predecessors shouldBe empty

    //block2.lineRange shouldBe Range.inclusive(32, 32)
    block2.successors shouldEqual List(block3)
    block2.predecessors shouldEqual List(block1)

    block3.lineRange shouldBe Range.inclusive(34, 35)
    block3.predecessors shouldEqual List(block2)
    block3.successors shouldEqual List(block4)

    block4.lineRange shouldBe Range.inclusive(36, 37)
    block4.predecessors shouldEqual List(block3)
    block4.successors shouldBe empty
  }

  it should "have successors and predecessors in tryCatchFinally" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatchFinally").get
    val block1 :: block2 :: block3 :: block4 :: block5 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(39, 40)
    block1.successors shouldEqual List(block2)
    block1.predecessors shouldBe empty

    block2.lineRange shouldBe Range.inclusive(42, 42)
    block2.successors shouldEqual List(block3, block4)
    block2.predecessors shouldEqual List(block1)

    block3.lineRange shouldBe Range.inclusive(43, 44)
    block3.successors shouldEqual List(block4)
    block3.predecessors shouldEqual List(block2)

    block4.lineRange shouldBe Range.inclusive(46, 47)
    block4.successors shouldEqual List(block5)
    block4.predecessors shouldEqual List(block2, block3)

    block5.lineRange shouldBe Range.inclusive(48, 49)
    block5.successors shouldBe empty
    block5.predecessors shouldEqual List(block4)
  }

  it should "have successors and predecessors in tryCatchCatchFinally" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatchCatchFinally").get
    val block1 :: block2 :: block3 :: block4 :: block5 :: block6 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(51, 52)
    block1.successors shouldEqual List(block2)
    block1.predecessors shouldBe empty

    block2.lineRange shouldBe Range.inclusive(54, 54)
    block2.successors shouldEqual List(block3, block4, block5)
    block2.predecessors shouldEqual List(block1)

    block3.lineRange shouldBe Range.inclusive(55, 56)
    block3.successors shouldEqual List(block5)
    block3.predecessors shouldEqual List(block2)

    block4.lineRange shouldBe Range.inclusive(57, 58)
    block4.successors shouldEqual List(block5)
    block4.predecessors shouldEqual List(block2)

    block5.lineRange shouldBe Range.inclusive(60, 61)
    block5.successors shouldEqual List(block6)
    block5.predecessors shouldEqual List(block2, block3, block4)

    block6.lineRange shouldBe Range.inclusive(62, 63)
    block6.successors shouldBe empty
    block6.predecessors shouldEqual List(block5)
  }

  it should "have successors and predecessors in tryCatch" in {
    val clazz = new Clazz("misc.BasicBlockExamples")
    val method = clazz.method("tryCatch").get
    val block1 :: block2 :: block3 :: block4 :: Nil = method.basicBlocks

    block1.lineRange shouldBe Range.inclusive(92, 93)
    block1.predecessors shouldBe empty
    block1.successors shouldEqual List(block2)

    block2.lineRange shouldBe Range.inclusive(95, 95)
    block2.predecessors shouldEqual List(block1)
    block2.successors shouldEqual List(block3, block4)

    block3.lineRange shouldBe Range.inclusive(96, 97)
    block3.predecessors shouldEqual List(block2)
    block3.successors shouldEqual List(block4)

    block4.lineRange shouldBe Range.inclusive(99, 100)
    block4.predecessors shouldEqual List(block2, block3)
    block4.successors shouldBe empty
  }

}