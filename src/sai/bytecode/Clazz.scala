package sai.bytecode

import org.apache.bcel.Repository
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic.ConstantPoolGen

class Clazz(val name: String) {
  val bcelClass: JavaClass = Repository.lookupClass(name)
  val cpg: ConstantPoolGen = new ConstantPoolGen(bcelClass.getConstantPool)

  val classType = org.apache.bcel.generic.Type.getType(Class.forName(name)).asInstanceOf[org.apache.bcel.generic.ReferenceType]

  val methods: List[Method] =
    for (bcelMethod <- bcelClass.getMethods.toList) yield new Method(bcelMethod, cpg, this)

  private def getMainMethod = method("main")

  def method(name: String): Option[Method] = methods.find(_.name == name)

  def interpret(): Unit = getMainMethod match {
    case Some(s) => s.interpret
    case None => println("class " + name + " doesn't contain main method")
  }

  def superClasses: List[Class[_]] = {
    def collectSuperClasses(cl: Class[_]): List[Class[_]] = {
      if (cl == null) Nil else cl :: collectSuperClasses(cl.getSuperclass)
    }
    collectSuperClasses(Class.forName(name).getSuperclass)
  }

}