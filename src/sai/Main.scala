package sai

import sai.bytecode.Program

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Main {

  def main(args: Array[String]): Unit = {

    val callGraphExamples = Program.getClass("sai.CallGraphExamples")

    val calculations = for {
      method <- scala.util.Random.shuffle(callGraphExamples.methods)
    } yield Future {
      val t0 = System.currentTimeMillis()
      val summary = method.summary
      val t1 = System.currentTimeMillis()
      (t1 - t0, method.id, summary)
    }

    val aggregated = Future.sequence(calculations)
    val result = Await.result(aggregated, Duration.Inf)
    result.foreach { case (calcTime, methodName, _) =>
        println(s"$methodName: summary information calculation time $calcTime (ms)")
    }

  }

}
