package scala

import bytecode.CallGraph
import org.scalatest.{FlatSpec, Matchers}
import sai.bytecode.Clazz

class CallGraphTest extends FlatSpec with Matchers {

  val clazz           = new Clazz("misc.CallGraphExample")
  val bound           = clazz.method("bound").get
  val min             = clazz.method("min").get
  val max             = clazz.method("max").get
  val log             = clazz.method("log").get
  val directRecursive = clazz.method("directRecursive").get
  val x               = clazz.method("x").get
  val y               = clazz.method("y").get
  val factorial       = clazz.method("factorial").get

  "A call graph" should "give you all successors of a method" in {
    val pcg           = CallGraph(bound)
    val subProcedures = pcg.getSuccessors(bound)
    subProcedures shouldEqual min :: max :: Nil
  }

  it should "give you all successors of a method recursive" in {
    val pcg           = CallGraph(bound)
    val subProcedures = pcg.getSuccessorsRecursive(bound)
    subProcedures shouldEqual log :: max :: min :: Nil
  }

  it should "determine if a procedure is direct recursive" in {
    val pcg = CallGraph(directRecursive)
    pcg.isRecursive(directRecursive) shouldBe true

    val pcg2 = CallGraph(bound)
    pcg2.isRecursive(bound) shouldBe false
  }

  it should "determine if a procedure is mutual recursive" in {
    val pcg = CallGraph(x)
    pcg.isRecursive(x) shouldBe true
    pcg.isRecursive(y) shouldBe true
  }

  it should "fff" in {
    val pcg = CallGraph(factorial)
    pcg.isRecursive(factorial) shouldBe true
    pcg.getSuccessors(factorial) shouldBe factorial :: Nil
    pcg.getSuccessorsRecursive(factorial) shouldBe factorial :: Nil
  }
}
