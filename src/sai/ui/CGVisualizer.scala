package ui

import cg._
import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.graph.implementations.SingleNode
import org.graphstream.graph.implementations.AbstractEdge

object CGVisualizer {

  def visualize(connectionGraph: ConnectionGraph) {

    val graph = new SingleGraph("graph")

    connectionGraph.nodes.foreach { node =>
      val id = node.toString
      if (graph.getNode[SingleNode](id) == null) {
        val singleNode: SingleNode = graph.addNode(id)
        singleNode.setAttribute("ui.label", nodeText(node) + connectionGraph.escapeMap(node))
        singleNode.setAttribute("ui.style", nodeStyle(node))
      }
    }

    connectionGraph.edges.foreach{ edge =>
      val directed = true
      val abstractEdge: AbstractEdge = graph.addEdge(edgeId(edge), edge.from.toString, edge.to.toString, directed)
      abstractEdge.setAttribute("ui.label", edgeText(edge))
    }

    graph.display()
  }

  private def edgeId(edge: Edge) = s"${edge.getClass.getSimpleName}(${edge.from.id} -> ${edge.to.id})"

  private def edgeText(edge: Edge) = edge match {
    case _: PointsToEdge => "P"
    case _: DeferredEdge => "D"
    case _: FieldEdge => "F"
  }

  private def nodeText(node: Node) = {
    val position = node.id.substring(node.id.lastIndexOf(",") + 1)
    val prefix = node match {
      case _: Phantom => "P"
      case _: ObjectNode => "O"
      case _: LocalReferenceNode => "L"
      case _: FieldReferenceNode => "F"
      case _: StaticReferenceNode => "S"
      case _: ActualReferenceNode => "A"
    }
    s"$prefix/$position"
  }

  private def nodeStyle(node: Node) = node match {
    case _: Phantom => "fill-color: gray;"
    case _: ObjectNode => "fill-color: blue;"
    case _: LocalReferenceNode => "fill-color: black;"
    case _: FieldReferenceNode => "fill-color: green;"
    case _: ActualReferenceNode => "fill-color: yellow;"
    case _: StaticReferenceNode => "fill-color: red;"
  }

}
