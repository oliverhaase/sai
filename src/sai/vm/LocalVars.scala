package sai.vm

class LocalVars(maxLocals: Int, argObjects: Map[Int, ObjectNode]) {
  val localVars: List[Slot] = build(maxLocals, argObjects)
  
  private def build(maxLocals: Int, argObjects: Map[Int, ObjectNode]): List[Slot] = {
    val builder:  Array[Slot] = new Array(maxLocals)
    for ( index <- 0 until maxLocals )
      if ( argObjects.contains(index) )
        builder(index) = argObjects.get(index).orNull
      else 
        builder(index) = PrimitiveSlot
    
    builder.toList
  }
  
  def get(index: Int) = localVars(index)
  
  def set(index: Int, slot: Slot) = localVars.updated(index, slot)
  

  
  // TODO override equals and hashcode
  
}