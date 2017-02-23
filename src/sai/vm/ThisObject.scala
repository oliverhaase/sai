package sai.vm

class ThisObject(referenceType: String) extends ObjectNode {
  override def toString = referenceType 
}