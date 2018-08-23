package sai.vm

import cg.ReferenceNode

sealed trait Slot {
}

sealed trait Reference extends Slot

case class ObjectRef(referenceType: org.apache.bcel.generic.Type, node: ReferenceNode) extends Reference {
  override def toString: String = s"$referenceType/$node"
}

object ObjectRef {
  val Null = ObjectRef(null, null)
}

case object DontCare extends Slot {
}
