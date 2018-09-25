package scala

import bytecode.GraphInfo
import org.scalatest.{FlatSpec, Matchers}

class GraphInfoTest extends FlatSpec with Matchers {

  "A GraphInfo" should "propagate 'impassable' information to successor and predecessor nodes" in {

    val graph = Map(
      'a' -> ('b' :: 'e' :: 'f' :: Nil),
      'b' -> ('c' :: 'e' :: Nil),
      'c' -> ('d' :: Nil),
      'd' -> ('j' :: Nil),
      'e' -> ('d' :: 'j' :: Nil),
      'f' -> ('g' :: 'h' :: Nil),
      'g' -> ('h' :: Nil),
      'h' -> ('i' :: Nil),
      'i' -> ('j' :: Nil),
      'j' -> Nil
    )

    val allNodes = graph.keys.toList

    def findSuccessors(c: Char) = graph(c)

    def findPredecessors(c: Char) =
      for {
        n <- allNodes if findSuccessors(n).contains(c)
      } yield n

    val impassableNodes = 'c' :: 'h' :: Nil

    val passableNodes =
      GraphInfo.findPassableNodes(allNodes, impassableNodes, findSuccessors, findPredecessors)
    passableNodes.sorted shouldBe List('a', 'b', 'd', 'e', 'j')
  }

  it should "return an empty list if there is no passable path in graph form top to bottom" in {
    val graph = Map(
      'a' -> ('b' :: 'c' :: Nil),
      'b' -> ('d' :: Nil),
      'c' -> ('d' :: 'e' :: Nil),
      'd' -> ('f' :: Nil),
      'e' -> ('f' :: Nil),
      'f' -> Nil
    )

    val allNodes = graph.keys.toList

    def findSuccessors(c: Char) = graph(c)

    def findPredecessors(c: Char) =
      for {
        node <- allNodes if findSuccessors(node).contains(c)
      } yield node

    val impassableNodes = 'c' :: 'd' :: Nil

    val passableNodes =
      GraphInfo.findPassableNodes(allNodes, impassableNodes, findSuccessors, findPredecessors)
    passableNodes.sorted shouldBe Nil
  }

}
