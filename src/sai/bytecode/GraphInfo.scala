package bytecode

import scala.annotation.tailrec

object GraphInfo {

  def findPassableNodes[T](allNodes: List[T],
                           impassableNodes: List[T],
                           findSuccessors: T => List[T],
                           findPredecessors: T => List[T]): List[T] = {

    val impassable = scala.collection.mutable.ListBuffer.empty[T]

    def isMarkedAsImpassable(node: T) = impassable.contains(node)

    def markAsImpassable(node: T): Unit = {
      if (!isMarkedAsImpassable(node)) {
        impassable += node

        // propagate impassable information to predecessors (backward propagation)
        val predecessors = findPredecessors(node)
        predecessors.foreach(predecessor => {
          if (findSuccessors(predecessor).forall(isMarkedAsImpassable)) {
            markAsImpassable(predecessor)
          }
        })

        // propagate impassable information to successors (forward propagation)
        val successors = findSuccessors(node)
        successors.foreach(successor => {
          if (findPredecessors(successor).forall(isMarkedAsImpassable)) {
            markAsImpassable(successor)
          }
        })
      }
    }

    impassableNodes.foreach(markAsImpassable)

    val passableNodes = allNodes.diff(impassable)
    passableNodes
  }

}
