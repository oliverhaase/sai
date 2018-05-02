import org.scalatest.FlatSpec
import org.scalatest.Matchers
import sai.bytecode.Clazz
import sai.bytecode.Method
import sai.bytecode.instruction.EntryPoint
import sai.bytecode.instruction.ExitPoint
import sai.vm.State
import sai.vm.PrimitiveSlot


class MethodTest extends FlatSpec with Matchers {

  val clazz = new Clazz("misc.SimpleMath")
  val calcNothing: Method = clazz.method("calcNothing").get
  val add: Method = clazz.method("add").get
  val meaningOfLife: Method = clazz.method("meaningOfLife").get

  "A Method" should "return a summary state with no local variables if a static void method without parameters is called" in {
    val states = calcNothing.summary
    states.size shouldBe 1
    val State(localVars, _, _) = states.toList.head
    localVars.localVars shouldBe empty
  }

  it should "return a summary state with local variables" in {
    val states = add.summary
    states.size shouldBe 1
    val State(localVars, _, _) = states.toList.head
    localVars.localVars.head.toString shouldBe "java.lang.Integer"
    localVars.localVars(1) shouldBe PrimitiveSlot
    localVars.localVars(2) shouldBe PrimitiveSlot
  }

  it should "return a summary state with the reference variable 'this' if the method is non-static" in {
    val states = meaningOfLife.summary
    states.size shouldBe 1
    val State(localVars, _, _) = states.toList.head
    localVars.localVars.size shouldBe 1
    localVars.localVars.head.toString shouldBe "misc.SimpleMath"
  }

  it should "give you the first instruction" in {
    add.firstInstruction should not be null
    add.firstInstruction should not be an [EntryPoint]
  }

  it should "have the a single ExitPoint as last instruction" in {
    add.exitPoint shouldBe an [ExitPoint]
    add.instructions.count(_.isInstanceOf[ExitPoint]) shouldBe 1
  }

}
