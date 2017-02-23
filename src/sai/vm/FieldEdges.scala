package sai.vm

case class FieldEdges(edges: Set[FieldEdge])

object FieldEdges {
  def apply() = new FieldEdges(Set())
}