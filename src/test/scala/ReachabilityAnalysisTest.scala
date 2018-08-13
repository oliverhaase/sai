package scala

import cg._
import org.scalatest.{FlatSpec, Matchers}

class ReachabilityAnalysisTest extends FlatSpec with Matchers {

  /**
    * S = Static Node (global escape)
    * A = Actual Node (arg escape)
    * L = Local Node  (no escape)
    */
  val S = StaticReferenceNode("S")
  val L = LocalReferenceNode("L")
  val A = ActualReferenceNode("A")

  "A ReachabilityAnalysis" should "update outgoing nodes with S -> L -> A" in {
    val cg = ConnectionGraph.empty()
      .addNodes(S, L, A)
      .addEdge(S -> L)
      .addEdge(L -> A)
      .updateEscapeState(S -> GlobalEscape)
      .updateEscapeState(L -> NoEscape)
      .updateEscapeState(A -> ArgEscape)

    val cg2 = ReachabilityAnalysis(cg)
    cg2.escapeMap shouldBe Map(S -> GlobalEscape, L -> GlobalEscape, A -> GlobalEscape)
  }

  it should "update outgoing nodes with A -> L -> S" in {
    val cg = ConnectionGraph.empty()
      .addNodes(S, L, A)
      .addEdge(A -> L)
      .addEdge(L -> S)
      .updateEscapeState(A -> ArgEscape)
      .updateEscapeState(L -> NoEscape)
      .updateEscapeState(S -> GlobalEscape)

    val cg2 = ReachabilityAnalysis(cg)
    cg2.escapeMap shouldBe Map(A -> ArgEscape, L -> ArgEscape, S -> GlobalEscape)
  }

  it should "update outgoing nodes with L -> L/S -> L" in {
    val L2 = LocalReferenceNode("L2")
    val L3 = LocalReferenceNode("L3")

    val cg = ConnectionGraph.empty()
      .addNodes(L, L2, S, L3)
      .addEdge(L -> L2)
      .addEdge(L -> S)
      .addEdge(L2 -> L3)
      .addEdge(S -> L3)
      .updateEscapeStates(Set(L, L2, L3) -> NoEscape)
      .updateEscapeState(S -> GlobalEscape)

    val cg2 = ReachabilityAnalysis(cg)
    cg2.escapeMap shouldBe Map(L -> NoEscape, L2 -> NoEscape, S -> GlobalEscape, L3 -> GlobalEscape)
  }



}
