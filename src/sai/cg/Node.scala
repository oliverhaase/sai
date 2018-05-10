package cg

import scala.runtime.ScalaRunTime


sealed trait Node {
  val uniqueId: String
  override def toString: String = s"${getClass.getSimpleName}($uniqueId)"
}
case class ObjectNode(uniqueId: String) extends Node

sealed trait ReferenceNode extends Node

trait Phantom { this: Product with ReferenceNode =>
  override def toString: String = ScalaRunTime._toString(this) + "/[phantom]"
}

case class LocalReferenceNode(uniqueId: String) extends ReferenceNode
case class ActualReferenceNode(uniqueId: String) extends ReferenceNode
case class FieldReferenceNode(uniqueId: String) extends ReferenceNode
case class GlobalReferenceNode(uniqueId: String) extends ReferenceNode
