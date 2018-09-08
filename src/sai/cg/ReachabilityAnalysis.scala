package cg

import implicits.MutableSetExtensions._

object ReachabilityAnalysis extends (ConnectionGraph => ConnectionGraph) {

  override def apply(cg: ConnectionGraph): ConnectionGraph = {

    val escapeStates = scala.collection.mutable.Map[Node, EscapeState](cg.escapeMap.toSeq: _*)
    val worklist = scala.collection.mutable.Set.empty[Node]

    // nodes escaping globally
    worklist ++= escapeStates.filter(_._2 == GlobalEscape).keys
    while (worklist.nonEmpty) {
      val node = worklist.removeAny()

      val outgoingNodes = cg.findOutgoingNodes(node)
      outgoingNodes.foreach { outgoingNode =>
        if (escapeStates(outgoingNode) != GlobalEscape) {
          escapeStates(outgoingNode) = GlobalEscape
          worklist += outgoingNode
        }
      }
    }

    // phantom argument nodes
    worklist ++= escapeStates.filter(_._2 == ArgEscape).keys
    while (worklist.nonEmpty) {
      val node = worklist.removeAny()

      val outgoingNodes = cg.findOutgoingNodes(node)
      outgoingNodes.foreach { outgoingNode =>
        if (escapeStates(outgoingNode) > ArgEscape) {
          escapeStates(outgoingNode) = ArgEscape
          worklist += outgoingNode
        }
      }
    }

    cg.copy(escapeMap = escapeStates.toMap)
  }

}
