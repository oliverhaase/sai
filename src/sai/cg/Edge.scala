package cg

sealed trait Edge {
  val from: Node
  val to: Node

  override def toString: String = s"${getClass.getSimpleName}($from -> $to)"
}

case class PointsToEdge private(from: ReferenceNode, to: ObjectNode) extends Edge

case class DeferredEdge private(from: ReferenceNode, to: ReferenceNode) extends Edge

case class FieldEdge private(from: ObjectNode, to: FieldReferenceNode) extends Edge

object PointsToEdge {
  def apply(fromTo: (ReferenceNode, ObjectNode)): PointsToEdge = PointsToEdge(fromTo._1, fromTo._2)
}

object DeferredEdge {
  def apply(fromTo: (ReferenceNode, ReferenceNode)): DeferredEdge = DeferredEdge(fromTo._1, fromTo._2)
}

object FieldEdge {
  def apply(fromTo: (ObjectNode, FieldReferenceNode)): FieldEdge = FieldEdge(fromTo._1, fromTo._2)
}