package cg

import cg.EscapeMap.EscapeMap

import scala.annotation.tailrec
import scala.collection.immutable.Set

case class ConnectionGraph(nodes: Set[Node], edges: Set[Edge], escapeMap: EscapeMap) {

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

  private def mergeEscapeSets(other: ConnectionGraph): EscapeMap = {
    val N1 = this.nodes
    val N2 = other.nodes
    val N3 = N1.union(N2)

    (for {
      n3 <- N3
      n1 = N1.find(n1 => n1 == n3)
      n2 = N2.find(n2 => n2 == n3)
      n3es = (n1, n2) match {
        case (Some(node1), Some(node2)) =>
          this.escapeMap(node1).merge(other.escapeMap(node2))
        case (Some(node1), _) =>
          this.escapeMap(node1)
        case (_, Some(node2)) =>
          other.escapeMap(node2)
        case _ => throw new IllegalStateException()
      }
    } yield (n3, n3es)).toMap

  }

  /**
    * Add a variable number of nodes to the CG.
    *
    * @param nodes variable argument list of nodes.
    * @return A connection graph with the added nodes.
    */
  def addNodes(nodes: Node*): ConnectionGraph = {
    copy(nodes = this.nodes ++ nodes)
  }

  def addNodes(nodes: Set[_ <: Node]): ConnectionGraph = {
    copy(nodes = this.nodes ++ nodes)
  }

  /**
    * Add a node to the CG.
    *
    * @param node Node to add to the graph.
    * @return A connection graph with the added node.
    */
  def addNode(node: Node): ConnectionGraph = {
    copy(nodes = nodes + node)
  }

  /**
    * Add an edge to the connection graph.
    *
    * @param fromTo Edge to add to the graph.
    * @return A connection graph with the added edge.
    */
  def addEdge(fromTo: (Node, Node)): ConnectionGraph = {
    val edge = fromTo match {
      case (local: LocalReferenceNode, obj: ObjectNode) => PointsToEdge(local -> obj)
      case (obj: ObjectNode, field: FieldReferenceNode) => FieldEdge(obj -> field)
      case (ref1: ReferenceNode, ref2: ReferenceNode) => DeferredEdge(ref1 -> ref2)
      case (from, to) => throw new IllegalArgumentException(s"cannot create an edge for types ${from.getClass.getSimpleName} -> ${to.getClass.getSimpleName}")
    }
    addEdge(edge)
  }

  /**
    * Add an edge to the connection graph.
    *
    * @param edge Edge to add to the graph.
    * @return A connection graph with the added edge.
    */
  def addEdge(edge: Edge): ConnectionGraph = {
    copy(edges = edges + edge)
  }

  def addEdges(edges: Set[_ <: Edge]): ConnectionGraph = {
    copy(edges = this.edges ++ edges)
  }

  /**
    * Kill local variable (i.e. bypass ingoing/outgoing edges).
    *
    * @param p local reference node to kill.
    * @return A connection graph with the localReferenceNode bypassed.
    */
  def byPass(p: LocalReferenceNode): ConnectionGraph = {
    val ingoingDeferredEdges = edges.collect {
      case edge@DeferredEdge(_, `p`) => edge
    }
    val outgoingPointsToEdges = edges.collect {
      case edge@PointsToEdge(`p`, _) => edge
    }
    val outgoingDeferredEdges = edges.collect {
      case edge@DeferredEdge(`p`, _) => edge
    }

    val bypassedPointsToEdges = for {
      in <- ingoingDeferredEdges
      out <- outgoingPointsToEdges
    } yield PointsToEdge(in.from -> out.to)

    val bypassedDeferredEdges = for {
      in <- ingoingDeferredEdges
      out <- outgoingDeferredEdges
    } yield DeferredEdge(in.from -> out.to)

    val edgesToRemove = ingoingDeferredEdges ++ outgoingPointsToEdges ++ outgoingDeferredEdges
    val edgesToAdd = bypassedPointsToEdges ++ bypassedDeferredEdges

    copy(edges = edges -- edgesToRemove ++ edgesToAdd)
  }

  /**
    * Update the escape state of a node by merging the old escape state with the new one.
    *
    * @param entry tuple with node and escape state
    * @return a connection graph with the updated escape state.
    */
  def updateEscapeState(entry: (Node, EscapeState)): ConnectionGraph = {
    val (node, escapeState) = entry
    updateEscapeStates(Set(node), escapeState)
  }

  def updateEscapeStates(entries: (Set[_ <: Node], EscapeState)): ConnectionGraph = {
    val (nodes: Set[Node], escapeState) = entries
    val states = for {
      node <- nodes
      es = determineEscapeState(node, escapeState)
    } yield (node, es)
    copy(escapeMap = escapeMap ++ states)
  }

  private def determineEscapeState(node: Node, escapeState: EscapeState): EscapeState = {
    escapeMap.get(node).fold(escapeState)(_ merge escapeState)
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
    * Mark all nodes as GlobalEscape.
    *
    * @return A new connection graph with all nodes marked as GlobalEscape.
    */
  def bottomSolution: ConnectionGraph = {
    val bottomSolution = for {
      (node, _) <- escapeMap
    } yield node -> GlobalEscape
    copy(escapeMap = bottomSolution)
  }

  def findByEscapeState(escapeState: EscapeState): Set[Node] = {
    for {
      node <- nodes if escapeMap(node) == escapeState
    } yield node
  }

  def findOutgoingNodes(node: Node): Set[Node] = {
    for {
      edge <- edges if edge.from == node
    } yield edge.to
  }



  /**
    * Check if a node is a 'terminal node'.
    * A node is called a 'terminal node' if it has no outgoing (points-to) edges.
    *
    * @param node Node to check.
    * @return true if node is a terminal node, false otherwise.
    */
  private def isTerminalNode(node: ReferenceNode) = pointsTo(node).isEmpty

  override def toString: String = s"Nodes: \n\t${nodes.mkString("\n\t")}\nEdges: \n\t${edges.mkString("\n\t")}\nEscapeSet: \n\t${escapeMap.mkString("\n\t")}"

}

object ConnectionGraph {
  def empty(): ConnectionGraph = new ConnectionGraph(Set(), Set(), EscapeMap())
}
