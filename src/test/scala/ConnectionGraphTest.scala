package scala

import cg.ConnectionGraph
import cg.DeferredEdge
import cg.PointsToEdge
import cg.Edge
import cg.Node
import cg.ObjectNode
import cg.LocalReferenceNode
import cg.GlobalReferenceNode
import cg.FieldReferenceNode
import cg.EscapeSet
import cg.EscapeStates._
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ConnectionGraphTest extends FlatSpec with Matchers {

  "A Connection Graph" should "find the points-to objects" in {
    /**
     * Example structure for test:
     * T a = new T("S1");
     * T b = a;
     * b = new T("S2");
     * T c = b;
     * T d = null;
     */
    val a = LocalReferenceNode("a")
    val b = LocalReferenceNode("b")
    val c = GlobalReferenceNode("c")
    val d = FieldReferenceNode("d")
    val s1 = ObjectNode("S1")
    val s2 = ObjectNode("S2")

    val nodes = Set[Node](a, b, c, d, s1, s2)
    val edges = Set[Edge](PointsToEdge(a -> s1), PointsToEdge(b -> s2), DeferredEdge(b -> a), DeferredEdge(c -> b))

    val cg = ConnectionGraph(nodes, edges, EscapeSet())
    cg.pointsTo(a) shouldBe Set(s1)
    cg.pointsTo(b) shouldBe Set(s1, s2)
    cg.pointsTo(c) shouldBe Set(s1, s2)
    cg.pointsTo(d) shouldBe Set()
  }

  it should "have a pretty 'toString' representation" in {
    val a = LocalReferenceNode("a")
    val b = ObjectNode("b")

    val nodes = Set[Node](a, b)
    val edges = Set[Edge](PointsToEdge(a, b))

    val cg = ConnectionGraph(nodes, edges, EscapeSet())
    val stringRepresentation = cg.toString
    stringRepresentation.isEmpty shouldBe false
    println(cg.toString)
  }

  it should "merge with another connection graph" in {
    val o1 = ObjectNode("1")
    val o2 = ObjectNode("2")
    val o3 = ObjectNode("3")

    val cg1 = ConnectionGraph(Set(o1, o2), Set(), EscapeSet(o1 -> NoEscape, o2 -> GlobalEscape))
    val cg2 = ConnectionGraph(Set(o2, o3), Set(), EscapeSet(o2 -> NoEscape, o3 -> ArgEscape))
    val mergedCG = cg1.merge(cg2)

    mergedCG.nodes shouldEqual Set(o1, o2, o3)
    mergedCG.edges shouldBe empty
    mergedCG.escapeSet shouldEqual EscapeSet(o1 -> NoEscape, o2 -> GlobalEscape, o3 -> ArgEscape)
  }

  it should "bypass a local variable" in {
    val o1 = ObjectNode("o1")
    val oN = ObjectNode("oN")
    val p = LocalReferenceNode("p")
    val s1 = LocalReferenceNode("s1")
    val sK = LocalReferenceNode("sK")
    val r1 = LocalReferenceNode("r1")
    val rM = LocalReferenceNode("rM")
    val nodesBeforeBypass: Set[Node] = Set(o1, oN, p, s1, sK, r1, rM)

    val edgesBeforeBypass = Set[Edge](
      DeferredEdge(s1 -> p),
      DeferredEdge(sK -> p),
      DeferredEdge(p -> r1),
      DeferredEdge(p -> rM),
      PointsToEdge(p -> o1),
      PointsToEdge(p -> oN)
    )

    val cgBefore = ConnectionGraph(nodesBeforeBypass, edgesBeforeBypass, EscapeSet())
    val cgAfter = cgBefore.byPass(p)
    cgAfter.nodes shouldEqual nodesBeforeBypass
    cgAfter.edges shouldEqual Set[Edge](
      DeferredEdge(s1 -> r1),
      DeferredEdge(s1 -> rM),
      DeferredEdge(sK -> r1),
      DeferredEdge(sK -> rM),
      PointsToEdge(s1 -> o1),
      PointsToEdge(s1 -> oN),
      PointsToEdge(sK -> o1),
      PointsToEdge(sK -> oN)
    )
  }

}
