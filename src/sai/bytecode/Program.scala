package sai.bytecode

object Program {

  private type ClassName = String

  private val classes = scala.collection.concurrent.TrieMap.empty[ClassName, Clazz]

  def getClass(name: ClassName) = classes.getOrElseUpdate(name, new Clazz(name))

}
