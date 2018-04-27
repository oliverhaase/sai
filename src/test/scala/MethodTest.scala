import org.scalatest.FlatSpec
import org.scalatest.Matchers
import sai.bytecode.Clazz


class MethodTest extends FlatSpec with Matchers {

  "A method" should "give you a summary" in {
    val clazz = new Clazz("jtree.TreeAnalyzer")
    val method = clazz.method("isLeaf").get

    val states = method.summary
    states.isEmpty shouldBe true
  }

}
