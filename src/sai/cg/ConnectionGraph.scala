package cg

import scala.annotation.tailrec
import scala.collection.immutable.Set

import cg.EscapeSet.EscapeSet

case class ConnectionGraph(nodes: Set[Node], edges: Set[Edge], escapeSet: EscapeSet) {

  /**
   * Merge connection graph with another connection graph.
   *
   * @param other Connection graph to merge.
   * @return A new connection graph which represents the merge.
   */
  def merge(other: ConnectionGraph): ConnectionGraph = {
    val newNodes = nodes.union(other.nodes)
    val newEdges = edges.union(other.edges)
    val newEscapeSet = mergeEscapeSets(other)
    ConnectionGraph(newNodes, newEdges, newEscapeSet)
  }

  private def mergeEscapeSets(other: ConnectionGraph): EscapeSet = {
    val N1 = this.nodes
    val N2 = other.nodes
    val N3 = N1.union(N2)

    (for (n3 <- N3)
      yield {
        val n1 = N1.find(n1 => n1.id == n3.id)
        val n2 = N2.find(n2 => n2.id == n3.id)
        val n3es = (n1, n2) match {
          case (Some(node1), Some(node2)) =>
            EscapeStates.merge(this.escapeSet(node1), other.escapeSet(node2))
          case (Some(node1), _) =>
            this.escapeSet(node1)
          case (_, Some(node2)) =>
            other.escapeSet(node2)
        }
        (n3, n3es)
      }).toMap
  }

  /**
   * Add a node if the graph does not already contain it.
   *
   * @param node Node to add to the graph.
   * @return A connection graph with the added node.
   */
  def addNode(node: Node): ConnectionGraph = {
    if (nodes.exists(_.id == node.id)) {
      this
    } else {
      copy(nodes = nodes + node)
    }
  }

  def addPointsToEdge(pointsToEdge: PointsToEdge): ConnectionGraph = {
    this
  }

  /**
   * Find all object nodes with a points-to path of length one.
   *
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
   *
   * @param node Node to check.
   * @return true if node is a terminal node, false otherwise.
   */
  private def isTerminalNode(node: ReferenceNode) = pointsTo(node).isEmpty

  override def toString: String = s"Nodes: \n\t${nodes.mkString("\n\t")}\nEdges: \n\t${edges.mkString("\n\t")}"

}

object ConnectionGraph {
  def empty(): ConnectionGraph = new ConnectionGraph(Set(), Set(), EscapeSet())
}
