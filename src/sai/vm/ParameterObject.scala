package sai.vm

class ParameterObject(referenceType: org.apache.bcel.generic.ReferenceType) extends ObjectNode {
  override def toString = referenceType toString
}