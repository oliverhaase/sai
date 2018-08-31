package scala

import org.scalatest.{FlatSpec, Matchers}
import sai.vm.{DontCare, OpStack}
import sai.vm.ObjectRef.Null


class OpStackTest extends FlatSpec with Matchers {

  "A stack" should "have a depth" in {
    var stack = OpStack()
    stack.depth shouldBe 0
    stack = stack.push(DontCare)
    stack.depth shouldBe 1
    stack = stack.push(Null)
    stack.depth shouldBe 2
    val emptyStack = stack.pop(2)
    emptyStack.depth shouldBe 0
  }

  it should "push values" in {
    var stack = OpStack()
    stack = stack.push(Null)
    stack.peek shouldBe Null
    stack = stack.push(DontCare)
    stack.peek shouldBe DontCare
  }

  it should "pop values" in {
    var stack = OpStack()
    stack = stack.push(Null).push(Null).push(DontCare)
    stack.pop shouldBe OpStack(Null :: Null :: Nil)
    stack.pop(3) shouldBe OpStack()
  }

  it should "throw an exception if popped is called on an empty stack" in {
    val stack = OpStack()
    an [Exception] should be thrownBy stack.pop
  }

  it should "throw an exception if popped is called more times than elements exist" in {
    var stack = OpStack()
    stack = stack.push(DontCare).push(DontCare).push(DontCare)
    an [Exception] should be thrownBy stack.pop(4)
  }

  it should "swap the two top stack values" in {
    val stack = OpStack(DontCare :: Null :: Nil)
    stack.swap shouldBe OpStack(Null :: DontCare :: Nil)
  }

}
