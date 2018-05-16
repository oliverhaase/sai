package scala

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import sai.util.Stack

class StackTest extends FlatSpec with Matchers {

  "A stack" should "have a depth" in {
    var stack = Stack[Int]()
    stack.depth shouldBe 0
    stack = stack.push(42)
    stack.depth shouldBe 1
    stack = stack.push(98)
    stack.depth shouldBe 2
    val (_, emptyStack) = stack.pop(2)
    emptyStack.depth shouldBe 0
  }

  it should "push values" in {
    var stack = Stack[Int]()
    stack = stack.push(5)
    stack.peek shouldBe 5
    stack = stack.push(9)
    stack.peek shouldBe 9
  }

  it should "pop values" in {
    var stack = Stack[Int]()
    stack = stack.push(1).push(2).push(3)
    stack.pop shouldBe (3, _: Stack[Int])
    stack.pop(2) shouldBe (List(2, 3), _: Stack[Int])
    stack.pop(3) shouldBe (List(1, 2, 3), _: Stack[Int])
  }

  it should "duplicate its top value" in {
    var stack = Stack[Int]()
    stack = stack.push(1)
    stack = stack.dup
    stack.depth shouldBe 2
    stack.peek shouldBe 1
  }

  it should "throw an exception if popped is called on an empty stack" in {
    val stack = Stack[Int]()
    an [Exception] should be thrownBy stack.pop
  }

  it should "throw an exception if dup is called on an empty stack" in {
    val stack = Stack[Int]()
    an [Exception] should be thrownBy stack.dup
  }

  it should "throw an exception if popped is called more times than elements exist" in {
    var stack = Stack[Int]()
    stack = stack.push(1).push(2).push(3)
    an [Exception] should be thrownBy stack.pop(4)
  }

}
