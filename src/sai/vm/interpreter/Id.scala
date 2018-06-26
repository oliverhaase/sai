package vm.interpreter

private[interpreter] object Id {

  private val ids = scala.collection.mutable.Map.empty[org.apache.bcel.generic.Instruction, String]

  def apply(i: org.apache.bcel.generic.Instruction): String = {
    ids.getOrElseUpdate(i, ids.size.toString)
  }
}
