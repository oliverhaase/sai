package cg

import org.apache.bcel.generic.ReferenceType
import sai.bytecode.Method


sealed trait Node {
  val id: String
  override def toString: String = s"${getClass.getSimpleName}($id)"
}

case class ObjectNode(id: String) extends Node

sealed trait ReferenceNode extends Node
case class LocalReferenceNode(id: String) extends ReferenceNode
case class ActualReferenceNode(id: String) extends ReferenceNode
case class FieldReferenceNode(id: String) extends ReferenceNode
case class StaticReferenceNode(id: String) extends ReferenceNode

object LocalReferenceNode {
  def apply(actualReferenceNode: ActualReferenceNode) = new LocalReferenceNode(actualReferenceNode.id)
}

object ActualReferenceNode {
  def apply(method: Method, index: Int) = new ActualReferenceNode(s"${method.clazz.name}.${method.name},index=$index")
}

object FieldReferenceNode {
  def apply(objectNode: ObjectNode, fieldName: String) = new FieldReferenceNode(s"${objectNode.id}->$fieldName")
}

object StaticReferenceNode {
  def apply(referenceType: ReferenceType, fieldName: String) = new StaticReferenceNode(s"$referenceType->$fieldName")
}

sealed trait Phantom
class PhantomObjectNode(override val id: String) extends ObjectNode(id) with Phantom
class PhantomReturnNode(override val id: String) extends ActualReferenceNode(id) with Phantom
class PhantomReferenceNode(override val id: String) extends ReferenceNode with Phantom

object PhantomReferenceNode {
  def apply(actualReferenceNode: ActualReferenceNode): PhantomReferenceNode = new PhantomReferenceNode(actualReferenceNode.id)
}

object PhantomObjectNode {
  def apply(referenceNode: ReferenceNode): PhantomObjectNode =
    new PhantomObjectNode(s"PO/${referenceNode.id}")
}