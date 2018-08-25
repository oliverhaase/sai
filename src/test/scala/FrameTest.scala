package scala

import cg._
import org.scalatest.{FlatSpec, Matchers}
import sai.vm.{DontCare, LocalVars, ObjectRef, OpStack}
import vm.Frame
import sai.vm.ObjectRef.Null

class FrameTest extends FlatSpec with Matchers {

  "A frame" should "equal another frame if both frames are empty" in {
    val frame1 = new Frame(null, null, OpStack(), new LocalVars(Nil), ConnectionGraph.empty())
    val frame2 = new Frame(null, null, OpStack(), new LocalVars(Nil), ConnectionGraph.empty())
    frame1 shouldEqual frame2
  }

  it should "equal another frame if both frames contain the same values" in {

    val opStack1 = OpStack(DontCare :: Null :: Nil)
    val localVars1 = new LocalVars(Null :: Nil)
    val nodes1: Set[Node] = Set(ObjectNode("O1"), FieldReferenceNode("f"))
    val edges1: Set[Edge] = Set(FieldEdge(ObjectNode("O1") -> FieldReferenceNode("f")))
    val escapeMap1 = EscapeMap(ObjectNode("O1") -> ArgEscape, FieldReferenceNode("f") -> NoEscape)
    val cg1 = ConnectionGraph(nodes1, edges1, escapeMap1)
    val frame1 = new Frame(null, null, opStack1, localVars1, cg1)

    val opStack2 = OpStack(DontCare :: Null :: Nil)
    val localVars2 = new LocalVars(Null :: Nil)
    val nodes2: Set[Node] = Set(ObjectNode("O1"), FieldReferenceNode("f"))
    val edges2: Set[Edge] = Set(FieldEdge(ObjectNode("O1") -> FieldReferenceNode("f")))
    val escapeMap2 = EscapeMap(ObjectNode("O1") -> ArgEscape, FieldReferenceNode("f") -> NoEscape)
    val cg2 = ConnectionGraph(nodes2, edges2, escapeMap2)
    val frame2 = new Frame(null, null, opStack2, localVars2, cg2)

    frame1 shouldEqual frame2
  }
}
