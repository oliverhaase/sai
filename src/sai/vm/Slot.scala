package sai.vm

import cg.Node

sealed trait Slot

case class Reference(referenceType: org.apache.bcel.generic.Type, node: Node) extends Slot {
  override def toString: String = s"$referenceType/$node"
}

case object Null extends Slot

case object DontCare extends Slot
