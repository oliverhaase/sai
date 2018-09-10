package sai

import sai.bytecode.Clazz
import ui.CGVisualizer

object Main {

  def main(args: Array[String]): Unit = {

    val x = for {
      clazzName <- "sai.T" :: "sai.Arr" :: Nil
      clazz = new Clazz(clazzName)
      method <- clazz.methods
      methodName = method.name
    } yield (clazzName, methodName)


    x.foreach(y => {
      val t = new Thread(() => {
        val graph = CGVisualizer.visualize(y._1, y._2)
        Thread.sleep(5000)
        CGVisualizer.screenshot(graph, y._1, y._2)
      })
      t.start()
      t.join()
    })
  }

}