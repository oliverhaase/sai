package sai.vm

case class LocalVars(localVars: List[Slot]) {  

  def get(index: Int) = localVars(index)
  
  def set(index: Int, slot: Slot) = new LocalVars(localVars.updated(index, slot))  
  
}

object LocalVars {
  def apply(maxLocals: Int, argObjects: Map[Int, Reference]): LocalVars = {
    val builder:  Array[Slot] = new Array(maxLocals)

    for ( index <- 0 until maxLocals )
      if ( argObjects.contains(index) )
        builder(index) = argObjects.get(index).orNull
      else 
        builder(index) = PrimitiveSlot

    new LocalVars(builder.toList)
  }
}

