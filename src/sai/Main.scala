package sai

import cg.StaticReferenceNode
import sai.bytecode.Program
import ui.CGVisualizer

object Main {

  def main(args: Array[String]): Unit = {

    val staticReferenceNodes = Map(
      "a"         -> 14,
      "b"         -> 14,
      "c"         -> 14,
      "d"         -> 11,
      "isEven"    -> 6,
      "isOdd"     -> 6,
      "factorial" -> 3
    )

    for {
      methodName <- "d" :: scala.util.Random.shuffle(staticReferenceNodes.keySet.toList)
      method     <- Program.getClass("sai.RecursiveExample").method(methodName)
    } {
      println(method.name)
      assert(
        method.summary.nodes.count(_.isInstanceOf[StaticReferenceNode]) == staticReferenceNodes(methodName)
      )
    }

    /*val clazz = Program.getClass("sai.RecursiveExample")
    val m = clazz.method("isEven").get
    CGVisualizer.visualize(m.summary).display()
    */

  }

}
