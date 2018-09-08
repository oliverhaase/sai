package sai

import sai.bytecode.Clazz
import ui.CGVisualizer

object Main {

  def main(args: Array[String]): Unit = {

    val graphs = for {
      clazzName <- "sai.T" :: "sai.Arr" :: Nil
      clazz = new Clazz(clazzName)
      method <- clazz.methods
    } yield (method.name, method.summary)
    graphs.foreach(println)

    CGVisualizer.visualize("sai.T", "returnOtherOrNull")

  }

}