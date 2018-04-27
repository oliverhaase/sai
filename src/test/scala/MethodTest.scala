import org.scalatest.FlatSpec
import org.scalatest.Matchers
import sai.bytecode.Clazz
import sai.vm.State
import sai.vm.PrimitiveSlot


class MethodTest extends FlatSpec with Matchers {

  "A Method" should "return a summary state with no local variables if a static void method without parameters is called" in {
    val clazz = new Clazz("misc.SimpleMath")
    val method = clazz.method("calcNothing").get

    val states = method.summary
    states.size shouldBe 1
    val State(localVars, opStack, edges) = states.toList.head
    localVars.localVars shouldBe empty
    opStack.stack.size shouldBe 0
    edges.edges shouldBe empty
  }

  it should "return a summary state with local variables" in {
    val clazz = new Clazz("misc.SimpleMath")
    val method = clazz.method("add").get

    val states = method.summary
    states.size shouldBe 1
    val State(localVars, opStack, edges) = states.toList.head
    localVars.localVars.head.toString shouldBe "java.lang.Integer"
    localVars.localVars(1) shouldBe PrimitiveSlot
    localVars.localVars(2) shouldBe PrimitiveSlot
    opStack.stack.size shouldBe 0
    edges.edges shouldBe empty
  }

  it should "return a summary state with the reference variable 'this' if the method is non-static" in {
    val clazz = new Clazz("misc.SimpleMath")
    val method = clazz.method("meaningOfLife").get

    val states = method.summary
    states.size shouldBe 1
    val State(localVars, opStack, edges) = states.toList.head
    localVars.localVars.size shouldBe 1
    localVars.localVars.head.toString shouldBe "misc.SimpleMath"
    opStack.stack shouldBe empty
    edges.edges shouldBe empty
  }

}
