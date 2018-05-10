package cg

sealed trait Edge {
  val from: Node
  val to: Node
  override def toString: String = s"${getClass.getSimpleName}($from -> $to)"
}

case class PointsToEdge(from: ReferenceNode, to: ObjectNode) extends Edge
case class DeferredEdge(from: ReferenceNode, to: ReferenceNode) extends Edge
case class FieldEdge(from: ObjectNode, to: FieldReferenceNode) extends Edge
