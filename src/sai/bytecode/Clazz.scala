package sai.bytecode

import org.apache.bcel.Repository
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic.ConstantPoolGen

class Clazz(val name: String) {
  val bcelClass: JavaClass = Repository.lookupClass(name) 
  val cpg: ConstantPoolGen = new ConstantPoolGen(bcelClass.getConstantPool());

  val methods: List[Method] = 
    for ( bcelMethod <-  bcelClass.getMethods.toList ) yield new Method(bcelMethod, cpg, this)

  private def getMainMethod = methods find (_.name == "main")
      
  def interpret = getMainMethod match {
      case Some(s) => s interpret  
      case None => println("class " + name + " doesn't contain main method")
    }
  
}