package cg

object EscapeMap {

  type EscapeMap = Map[Node, EscapeState]

  def apply(): EscapeMap = Map.empty[Node, EscapeState]

  def apply(data: (Node, EscapeState)*): EscapeMap = data.toMap
}
