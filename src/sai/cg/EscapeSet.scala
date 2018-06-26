package cg

import cg.EscapeStates.EscapeState

object EscapeSet {
  type EscapeSet = Map[Node, EscapeState]
  def apply(): EscapeSet = Map.empty[Node, EscapeState]
  def apply(data: (Node, EscapeState)*): EscapeSet = data.toMap
}
