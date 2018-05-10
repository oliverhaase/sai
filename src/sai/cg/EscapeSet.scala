package cg

import cg.EscapeSet.EscapeSet
import cg.EscapeStates.EscapeState
import cg.EscapeStates._

object EscapeSet {
  type EscapeSet = scala.collection.immutable.HashMap[Node, EscapeState]
}
