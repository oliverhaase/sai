package sai.vm

import sai.vm.Reference.Null

case class LocalVars(localVars: List[Slot]) {

  def get(index: Int) = localVars(index)

  def set(index: Int, slot: Slot) = new LocalVars(localVars.updated(index, slot))

  def merge(other: LocalVars): LocalVars = {
    val slots = for {
      (mySlot, otherSlot) <- localVars.zipAll(other.localVars, MultivalueSlot(), MultivalueSlot())
      merged = mySlot.merge(otherSlot)
    } yield merged
    LocalVars(slots)
  }

}

object LocalVars {
  def apply(maxLocals: Int, argObjects: Map[Int, Reference]): LocalVars = {
    val builder: Array[Slot] = new Array(maxLocals)

    for (index <- 0 until maxLocals)
      if (argObjects.contains(index))
        builder(index) = argObjects.getOrElse(index, Null)
      else
        builder(index) = DontCare

    new LocalVars(builder.toList)
  }
}