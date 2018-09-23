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
    CallGraph(bound).getSuccessors() shouldEqual min :: max :: Nil
  }

  it should "give you all successors of a method recursive" in {
    CallGraph(bound).recursive() shouldEqual log :: max :: min :: Nil
  }

  it should "determine if a procedure is recursive" in {
    CallGraph(directRecursive).isRecursive() shouldBe true
    CallGraph(x).isRecursive() shouldBe true
    CallGraph(bound).isRecursive() shouldBe false
  }

  it should "fff" in {
    val callGraph = CallGraph(factorial)
    callGraph.isRecursive() shouldBe true
    callGraph.getSuccessors() shouldBe factorial :: Nil
    callGraph.recursive() shouldBe factorial :: Nil
  }

}
