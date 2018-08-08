package sai.vm

import cg.ReferenceNode

import scala.annotation.tailrec
import scala.collection.immutable.Set

sealed trait Slot {

  def merge(other: Slot): Slot = other match {
    case slot if this == slot => this
    case slot: MultivalueSlot => slot.merge(this)
    case _ => MultivalueSlot(Set(this, other))
  }

  def map[B](fun: Slot => B): Set[B] = Set(fun(this))

  def flatMap[B](fun: (Slot) => scala.collection.GenTraversableOnce[B]): Set[B] = map(fun).flatten

}

case class Reference(referenceType: org.apache.bcel.generic.Type, node: ReferenceNode) extends Slot {
  override def toString: String = s"$referenceType/$node"
}

object Reference {
  val Null = Reference(null, null)
}

case object DontCare extends Slot {
}

case class MultivalueSlot(slots: Set[Slot]) extends Slot {

  override def map[B](fun: Slot => B): Set[B] = slots.map(fun)

  override def merge(other: Slot): Slot = {
    val otherSlots = other.map(identity)
    val flattened = flatten(this.slots ++ otherSlots, Set.empty[Slot])
    MultivalueSlot(flattened)
  }

  @tailrec
  private def flatten(slots: Set[Slot], result: Set[Slot]): Set[Slot] = slots.toList match {
    case Nil => result
    case (slot: MultivalueSlot) :: rest => flatten(slot.slots ++ rest, result)
    case slot :: rest => flatten(rest.toSet, result + slot)
  }

}

object MultivalueSlot {
  def apply(slot: Slot*): Slot = slot.reduce(_ merge _)

  def apply(): Slot = MultivalueSlot(Set.empty[Slot])
}