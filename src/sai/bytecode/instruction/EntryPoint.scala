package sai.bytecode.instruction

import sai.bytecode.Method
import sai.vm._

class EntryPoint(method: Method) extends Instruction(null, null, method) {
   override def next: Instruction = method firstInstruction
   
   override def successors: List[Instruction] = List(next)

  override def predecessors: Set[Instruction] = Set()
   
   override def statesIn: Set[State] = throw new RuntimeException("this value is not supposed to be used")   

   override def statesOut: Set[State] = 
     Set(State(LocalVars(method maxLocals, method inputReferences), OpStack(), FieldEdges()))
  
   override def toString = "entry point"
}