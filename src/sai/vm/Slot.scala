package sai.vm

import cg.ReferenceNode

sealed trait Slot {
}

case class Reference(referenceType: org.apache.bcel.generic.Type, node: ReferenceNode) extends Slot {
  override def toString: String = s"$referenceType/$node"
}

object Reference {
  val Null = Reference(null, null)
}

case object DontCare extends Slot {
}
