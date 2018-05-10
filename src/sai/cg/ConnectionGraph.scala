package cg

import scala.annotation.tailrec
import scala.collection.immutable.Set

case class ConnectionGraph(nodes: Set[Node], edges: Set[Edge]) {

  /**
   * Merge connection graph with another connection graph.
   * @param other Connection graph to merge.
   * @return A new connection graph which represents the merge.
   */
  def merge(other: ConnectionGraph): ConnectionGraph = {
    new ConnectionGraph(nodes.union(other.nodes), edges.union(other.edges))
  }

  /**
   * Find all object nodes with a points-to path of length one.
   * @param node Node for which the points-to analysis will be performed.
   * @return A set of object nodes that are connected to <code>node</code> with exactly one points-to path.
   */
  def pointsTo(node: ReferenceNode): Set[ObjectNode] = {

    @tailrec
    def pointsToRec(nodes: List[ReferenceNode], objects: Set[ObjectNode]): Set[ObjectNode] = nodes match {
      case Nil => objects
      case m :: tail =>
        val pointsTo = edges.collect {
          case PointsToEdge(`m`, n) => n
        }
        val deferred = edges.collect {
          case DeferredEdge(`m`, to) => to
        }
        pointsToRec(tail ++ deferred, objects ++ pointsTo)
    }

    pointsToRec(List(node), objects = Set())
  }

  /**
   * Check if a node is a 'terminal node'.
   * A node is called a 'terminal node' if it has no outgoing (points-to) edges.
   * @param node Node to check.
   * @return true if node is a terminal node, false otherwise.
   */
  private def isTerminalNode(node: ReferenceNode) = pointsTo(node).isEmpty

  override def toString: String = s"Nodes: \n\t${nodes.mkString("\n\t")}\nEdges: \n\t${edges.mkString("\n\t")}"

}

object ConnectionGraph {
  def apply(): ConnectionGraph = new ConnectionGraph(Set(), Set())
}
