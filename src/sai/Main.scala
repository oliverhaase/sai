package sai

import sai.bytecode.Program
import sai.bytecode.Clazz

object Main {
  def main(args: Array[String]) = {
    val clazz = new Clazz("sai.TestClass")
    Program.classes ::= clazz 
    clazz.interpret
  }
    
}