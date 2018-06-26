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
  def apply(tuple: (ReferenceNode, ObjectNode)): PointsToEdge = PointsToEdge(tuple._1, tuple._2)
}

object DeferredEdge {
  def apply(tuple: (ReferenceNode, ReferenceNode)): DeferredEdge = DeferredEdge(tuple._1, tuple._2)
}

object FieldEdge {
  def apply(tuple: (ObjectNode, FieldReferenceNode)): FieldEdge = FieldEdge(tuple._1, tuple._2)
}