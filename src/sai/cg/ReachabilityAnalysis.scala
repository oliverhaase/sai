package cg

object ReachabilityAnalysis extends (ConnectionGraph => ConnectionGraph) {

  override def apply(cg: ConnectionGraph): ConnectionGraph = {

    val escapeStates = scala.collection.mutable.Map[Node, EscapeState](cg.escapeMap.toSeq: _*)
    val worklist = scala.collection.mutable.Set.empty[Node]

    // nodes escaping globally
    worklist ++= cg.findByEscapeState(GlobalEscape)
    while (worklist.nonEmpty) {
      val node = worklist.head
      worklist -= node

      val outgoingNodes = cg.findOutgoingNodes(node)
      outgoingNodes.foreach { outgoingNode =>
        if (escapeStates(outgoingNode) != GlobalEscape) {
          escapeStates(outgoingNode) = GlobalEscape
          worklist += outgoingNode
        }
      }
    }

    // phantom argument nodes
    worklist ++= cg.findByEscapeState(ArgEscape)
    while (worklist.nonEmpty) {
      val node = worklist.head
      worklist -= node

      val outgoingNodes = cg.findOutgoingNodes(node)
      outgoingNodes.foreach { outgoingNode =>
        if (escapeStates(outgoingNode) > GlobalEscape) {
          escapeStates(outgoingNode) = ArgEscape
          worklist += outgoingNode
        }
      }
    }

    cg.copy(escapeMap = cg.escapeMap ++ escapeStates)
  }

}
