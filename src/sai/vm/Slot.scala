package sai.vm

sealed trait Slot

object PrimitiveSlot extends Slot {
  override def toString: String = "primitive"
}

case class ThisObject(referenceType: String) extends Slot {
  override def toString: String = referenceType
}

case class ObjectRef(referenceType: org.apache.bcel.generic.Type, name: String) extends Slot {
  override def toString: String = s"$referenceType/$name"
}

case class StaticRef(name: String) extends Slot {
  override def toString: String = s"static/$name"
}

