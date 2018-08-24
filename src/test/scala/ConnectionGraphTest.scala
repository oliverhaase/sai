package scala

import cg._
import org.scalatest.{FlatSpec, Matchers}

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
    val c = StaticReferenceNode("c")
    val d = FieldReferenceNode("d")
    val s1 = ObjectNode("S1")
    val s2 = ObjectNode("S2")

    val nodes = Set[Node](a, b, c, d, s1, s2)
    val edges = Set[Edge](PointsToEdge(a -> s1), PointsToEdge(b -> s2), DeferredEdge(b -> a), DeferredEdge(c -> b))

    val cg = ConnectionGraph(nodes, edges, EscapeMap())
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

    val cg = ConnectionGraph(nodes, edges, EscapeMap())
    val stringRepresentation = cg.toString
    stringRepresentation.isEmpty shouldBe false
    println(cg.toString)
  }

  it should "merge with another connection graph" in {
    val o1 = ObjectNode("1")
    val o2 = ObjectNode("2")
    val o3 = ObjectNode("3")

    val cg1 = ConnectionGraph(Set(o1, o2), Set(), EscapeMap(o1 -> NoEscape, o2 -> GlobalEscape))
    val cg2 = ConnectionGraph(Set(o2, o3), Set(), EscapeMap(o2 -> NoEscape, o3 -> ArgEscape))
    val mergedCG = cg1.merge(cg2)

    mergedCG.nodes shouldEqual Set(o1, o2, o3)
    mergedCG.edges shouldBe empty
    mergedCG.escapeMap shouldEqual EscapeMap(o1 -> NoEscape, o2 -> GlobalEscape, o3 -> ArgEscape)
  }

  it should "bypass a local variable" in {
    // the connection graph is structured as the passBy example in the paper
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

    val cgBefore = ConnectionGraph(nodesBeforeBypass, edgesBeforeBypass, EscapeMap())
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

  it should "update the escape state of a node" in {
    val objectNode = ObjectNode("O")
    var cg = ConnectionGraph.empty().addNode(objectNode)
    cg.escapeMap shouldBe empty

    cg = cg.updateEscapeState(objectNode -> NoEscape)
    cg.escapeMap should contain(objectNode -> NoEscape)

    cg = cg.updateEscapeState(objectNode -> GlobalEscape)
    cg.escapeMap should contain(objectNode -> GlobalEscape)

    cg = cg.updateEscapeState(objectNode -> ArgEscape)
    cg.escapeMap should contain(objectNode -> GlobalEscape)
  }

  it should "create a bottom solution" in {
    val locals: Set[Node] = Set(LocalReferenceNode("L1"), LocalReferenceNode("L2"))
    val o1 = ObjectNode("O1")
    val o2 = ObjectNode("O2")
    val objects: Set[Node] = Set(o1, o2)
    val statics: Set[Node] = Set(StaticReferenceNode("S1"), StaticReferenceNode("S2"))

    val cg =
      ConnectionGraph.empty()
        .addNodes(locals.union(objects).union(statics))
        .updateEscapeStates(locals -> NoEscape)
        .updateEscapeStates(objects -> ArgEscape)
        .updateEscapeStates(statics -> GlobalEscape)

    cg.escapeMap.values.toSet shouldBe Set(NoEscape, ArgEscape, GlobalEscape)
    cg.bottomSolution.escapeMap(o1) shouldBe GlobalEscape
    cg.bottomSolution.escapeMap(o2) shouldBe GlobalEscape
  }

  it should "find nodes by escape state" in {
    val L1 = LocalReferenceNode("L1")
    val L2 = LocalReferenceNode("L2")
    val A = ActualReferenceNode("A")
    val S = StaticReferenceNode("S")
    val cg =
      ConnectionGraph.empty()
        .addNodes(L1, L2, A, S)
        .updateEscapeState(L1 -> NoEscape)
        .updateEscapeState(L2 -> NoEscape)
        .updateEscapeState(A -> ArgEscape)
        .updateEscapeState(S -> GlobalEscape)
    cg.findByEscapeState(NoEscape) shouldBe Set(L1, L2)
  }

  it should "find outgoing nodes" in {
    val L1 = LocalReferenceNode("L1")
    val L2 = LocalReferenceNode("L2")
    val L3 = LocalReferenceNode("L3")
    val L4 = LocalReferenceNode("L4")
    val S = StaticReferenceNode("S")
    val cg =
      ConnectionGraph.empty()
        .addNodes(L1, L2, L3, L4, S)
        .addEdge(S -> L1)
        .addEdge(L1 -> L2)
        .addEdge(L1 -> L3)
        .addEdge(L3 -> L4)
    cg.findOutgoingNodes(L1) shouldBe Set(L2, L3)
  }

  it should "create a nonlocal subgraph" in {
    val L1 = LocalReferenceNode("L1")
    val L2 = LocalReferenceNode("L2")
    val L3 = LocalReferenceNode("L3")
    val A1 = ActualReferenceNode("A1")
    val A2 = ActualReferenceNode("A1")
    val S1 = StaticReferenceNode("S1")
    val S2 = StaticReferenceNode("S2")

    val cg = ConnectionGraph.empty()
      .addNodes(L1, L2, L3, A1, A2, S1, S2)
      .updateEscapeStates(Set(L1, L2, L3) -> NoEscape)
      .updateEscapeStates(Set(A1, A2) -> ArgEscape)
      .updateEscapeStates(Set(S1, S2) -> GlobalEscape)
      .addEdge(L1 -> L2)
      .addEdge(A1 -> L3)
      .addEdge(S1 -> L3)
    val nonlocalSubgraph = cg.nonlocalSubgraph
    nonlocalSubgraph.nodes shouldBe Set(A1, A2, S1, S2)
    nonlocalSubgraph.edges shouldBe Set(DeferredEdge(A1 -> L3), DeferredEdge(S1 -> L3))
    nonlocalSubgraph.escapeMap shouldBe Map(A1 -> ArgEscape, A2 -> ArgEscape, S1 -> GlobalEscape, S2 -> GlobalEscape)
  }

}
