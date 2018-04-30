package sai.vm

case class OpStack(stack: List[Slot]) {
  
  def push(slot: Slot): OpStack = new OpStack(slot :: stack)
  
  def push(slot: Slot, n: Int): OpStack = 
    if ( n > 0 ) 
      push(slot).push(slot, n -1)
    else
      this
      
  def pop: OpStack = new OpStack(stack.tail)
  
  def pop(n: Int): OpStack = 
    if ( n > 0 )
      pop.pop(n -1)
    else 
      this
      
  def top = stack.head
   
}

object OpStack {
  def apply() = new OpStack(Nil)
}