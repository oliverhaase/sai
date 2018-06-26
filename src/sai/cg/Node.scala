package cg

import scala.runtime.ScalaRunTime


sealed trait Node {
  val id: String
  override def toString: String = s"${getClass.getSimpleName}($id)"
}
case class ObjectNode(id: String) extends Node

sealed trait ReferenceNode extends Node

trait Phantom { this: Product with ReferenceNode =>
  override def toString: String = ScalaRunTime._toString(this) + "/[phantom]"
}

case class LocalReferenceNode(id: String) extends ReferenceNode
case class ActualReferenceNode(id: String) extends ReferenceNode
case class FieldReferenceNode(id: String) extends ReferenceNode
case class GlobalReferenceNode(id: String) extends ReferenceNode
