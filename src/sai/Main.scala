package sai

import sai.bytecode.Program
import sai.bytecode.Clazz

object Main {

  def main(args: Array[String]): Unit = {
    val clazz = new Clazz("sai.TestClass")
    Program.classes ::= clazz
    clazz.interpret()

    val myFileReader = new Clazz("sai.MyFileReader")
    myFileReader.method("readIntoArray").get.interpret

    val basicStatements = new Clazz("sai.BasicStatements")
    basicStatements.method("<init>").get.interpret
    basicStatements.method("<clinit>").get.interpret
    basicStatements.method("localAssignment").get.interpret
    basicStatements.method("instanceAssignment").get.interpret
    basicStatements.method("staticAssignment").get.interpret
    basicStatements.method("localDefer").get.interpret
  }

}