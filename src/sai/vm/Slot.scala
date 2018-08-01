package sai.vm

import cg.Node

sealed trait Slot

object PrimitiveSlot extends Slot {
  override def toString: String = "primitive"
}

case class Reference(referenceType: org.apache.bcel.generic.Type, node: Node) extends Slot {
  override def toString: String = s"$referenceType/$node"
}

case class ParameterObject(referenceType: org.apache.bcel.generic.Type) extends Slot {
  override def toString: String = s"$referenceType/param"
}

case class ThisObject(referenceType: String, node: Node) extends Slot {
  override def toString: String = referenceType
}

case object Null extends Slot

case object DontCare extends Slot


