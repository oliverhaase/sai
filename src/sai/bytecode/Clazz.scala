package sai.bytecode

import org.apache.bcel.Repository
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic.ConstantPoolGen

import scala.util.{Failure, Success, Try}

class Clazz(val name: String) {
  val bcelClass: JavaClass = Repository.lookupClass(name)
  val cpg: ConstantPoolGen = new ConstantPoolGen(bcelClass.getConstantPool)
  val isClass              = bcelClass.isClass
  val isInterface          = bcelClass.isInterface

  val classType = org.apache.bcel.generic.Type
    .getType(Class.forName(name))
    .asInstanceOf[org.apache.bcel.generic.ReferenceType]

  val methods: List[Method] =
    for {
      bcelMethod <- bcelClass.getMethods.toList
      method     = new Method(bcelMethod, cpg, this)
    } yield method

  def lookupMethod(name: String): Option[Method] = methods.find(_.name == name)

  def interpret(): Unit = lookupMethod("main") match {
    case Some(s) => s.interpret
    case None    => println("class " + name + " doesn't contain main method")
  }

  def superClasses: List[Clazz] = {
    def collectSuperClasses(cl: Class[_]): List[Clazz] = {
      if (cl == null)
        Nil
      else
        Program.getClass(cl.getCanonicalName) :: collectSuperClasses(cl.getSuperclass)
    }
    collectSuperClasses(Class.forName(name).getSuperclass)
  }

  def interfaces: List[Clazz] =
    for {
      interface        <- bcelClass.getAllInterfaces.toList
      interfaceAsClass = Program.getClass(interface.getClassName)
    } yield interfaceAsClass

}
