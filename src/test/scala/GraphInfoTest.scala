package scala

import bytecode.GraphInfo
import org.scalatest.{FlatSpec, Matchers}

class GraphInfoTest extends FlatSpec with Matchers {

  "A GraphInfo" should "find all paths in a graph" in {
    val graph = Map(
      1 -> (2 :: 3 :: 4 :: Nil),
      2 -> (3 :: 5 :: Nil),
      3 -> (6 :: Nil),
      4 -> (6 :: Nil),
      5 -> (6 :: Nil),
      6 -> Nil
    )

    val paths = GraphInfo.getPaths[Int](1, graph(_))
    paths shouldBe List(
      1 :: 2 :: 3 :: 6 :: Nil,
      1 :: 2 :: 5 :: 6 :: Nil,
      1 :: 3 :: 6 :: Nil,
      1 :: 4 :: 6 :: Nil
    )
  }

  it should "determine if a graph contains cycles or not" in {
    val cyclic1 = Map(
      1 -> (1 :: Nil)
    )
    GraphInfo.isCyclic(1, cyclic1(_)) shouldBe true

    val cyclic2 = Map(
      1 -> (2 :: Nil),
      2 -> (3 :: Nil),
      3 -> (1 :: Nil)
    )
    GraphInfo.isCyclic(1, cyclic2(_)) shouldBe true

    val cyclic3 = Map(
      1 -> (2 :: Nil),
      2 -> (1 :: Nil)
    )
    GraphInfo.isCyclic(1, cyclic3(_)) shouldBe true


    val nonCyclic1 = Map(
      1 -> List.empty[Int]
    )
    GraphInfo.isCyclic(1, nonCyclic1(_)) shouldBe false

    val nonCyclic2 = Map(
      1 -> (2 :: Nil),
      2 -> (3 :: Nil),
      3 -> Nil
    )
    GraphInfo.isCyclic(1, nonCyclic2(_)) shouldBe false

    val nonCyclic3 = Map(
      1 -> (2 :: 3 :: Nil),
      2 -> (3 :: 4 :: 5 :: Nil),
      3 -> (4 :: Nil),
      4 -> (6 :: Nil),
      5 -> (6 :: Nil),
      6 -> Nil
    )
    GraphInfo.isCyclic(1, nonCyclic3(_)) shouldBe false
  }

  it should "determine if there is a path from a node to another node" in {
    val graph = Map(
      1 -> (2 :: Nil),
      2 -> (2 :: 3 :: 4 :: Nil),
      3 -> (4 :: 5 :: 6 :: Nil),
      4 -> (7 :: 8 :: Nil),
      5 -> (3 :: Nil),
      6 -> Nil,
      7 -> Nil,
      8 -> Nil
    )

    GraphInfo.pathExists(1, 1, graph(_)) shouldBe false
    GraphInfo.pathExists(1, 2, graph(_)) shouldBe true
    GraphInfo.pathExists(1, 3, graph(_)) shouldBe true
    GraphInfo.pathExists(1, 4, graph(_)) shouldBe true
    GraphInfo.pathExists(1, 5, graph(_)) shouldBe true
    GraphInfo.pathExists(1, 6, graph(_)) shouldBe true
    GraphInfo.pathExists(1, 7, graph(_)) shouldBe true
    GraphInfo.pathExists(1, 8, graph(_)) shouldBe true

    GraphInfo.pathExists(2, 1, graph(_)) shouldBe false
    GraphInfo.pathExists(2, 2, graph(_)) shouldBe true
    GraphInfo.pathExists(2, 3, graph(_)) shouldBe true
    GraphInfo.pathExists(2, 4, graph(_)) shouldBe true
    GraphInfo.pathExists(2, 5, graph(_)) shouldBe true
    GraphInfo.pathExists(2, 6, graph(_)) shouldBe true
    GraphInfo.pathExists(2, 7, graph(_)) shouldBe true
    GraphInfo.pathExists(2, 8, graph(_)) shouldBe true

    GraphInfo.pathExists(3, 1, graph(_)) shouldBe false
    GraphInfo.pathExists(3, 2, graph(_)) shouldBe false
    GraphInfo.pathExists(3, 3, graph(_)) shouldBe true
    GraphInfo.pathExists(3, 4, graph(_)) shouldBe true
    GraphInfo.pathExists(3, 5, graph(_)) shouldBe true
    GraphInfo.pathExists(3, 6, graph(_)) shouldBe true
    GraphInfo.pathExists(3, 7, graph(_)) shouldBe true
    GraphInfo.pathExists(3, 8, graph(_)) shouldBe true

  }
}
