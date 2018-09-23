package sai.bytecode

object Program {

  private type ClassName = String

  private val classes = scala.collection.mutable.Map.empty[ClassName, Clazz]

  def getClass(name: ClassName) = classes.getOrElseUpdate(name, new Clazz(name))

}