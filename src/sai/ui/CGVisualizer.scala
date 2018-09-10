package ui

import java.io.File
import java.nio.file.{FileSystem, FileSystems}

import cg._
import javafx.scene.shape.Path
import org.graphstream.graph.implementations.{AbstractEdge, SingleGraph, SingleNode}
import org.graphstream.ui.spriteManager.SpriteManager
import org.graphstream.ui.view.Viewer
import sai.bytecode.Clazz

object CGVisualizer {

  def visualize(clazzName: String, methodName: String): SingleGraph = {
    val clazz = new Clazz(clazzName)
    val method = clazz.method(methodName).get
    val graph = visualize(method.summary)

    graph.addAttribute("ui.antialias")
    val v = graph.display()

    val spm = new SpriteManager(graph)
    val sp1 = spm.addSprite(methodName)
    sp1.addAttribute("ui.label", clazzName + " - " + methodName)
    sp1.addAttribute("ui.style", "fill-color: white;\n\ttext-style: bold-italic; text-size: 20;")

    sp1.setPosition(0, 0, 0)

    graph
  }

  def screenshot(graph: SingleGraph, clazzName: String, methodName: String): Unit = {
    val path = System.getProperty("user.home") + "/Desktop/graphs"
    val filename = s"$clazzName-$methodName.png"
    graph.addAttribute("ui.screenshot", s"$path/$filename")
  }


  def visualize(connectionGraph: ConnectionGraph): SingleGraph = {

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

    graph
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
      case _: PhantomObjectNode => "PO"
      case _: PhantomReturnNode => "PRet"
      case _: PhantomReferenceNode => "PRef"
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
