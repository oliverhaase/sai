package bytecode

import scala.annotation.tailrec

object GraphInfo {

  type Path[T] = List[T]

  def getPaths[T](head: T, findSuccessors: (T) => List[T]): List[Path[T]] = {
    def rec(xs: List[T]): List[Path[T]] = {
      findSuccessors(xs.last) match {
        case Nil =>
          xs :: Nil
        case successors =>
          successors.flatMap(x => rec(xs ::: x :: Nil))
      }
    }

    rec(head :: Nil)
  }

  def pathExists[T](from: T, to: T, findSuccessors: (T) => List[T]): Boolean = {
    val visited = scala.collection.mutable.Set.empty[T]

    def rec(x: T): Boolean = {
      visited += x
      findSuccessors(x) match {
        case Nil =>
          false
        case succs if succs.contains(to) =>
          true
        case succs =>
          val unvisited = succs.filterNot(visited.contains)
          unvisited.exists(rec)
      }
    }

    rec(from)
  }

  def isCyclic[T](head: T, findSuccessors: (T) => List[T]): Boolean = {

    val nodes        = getSuccessorsRecursive(head, findSuccessors)
    val combinations = for (n1 <- nodes; n2 <- nodes) yield (n1, n2)

    combinations.exists {
      case (n1, n2) =>
        pathExists(from = n1, to = n2, findSuccessors) &&
          pathExists(from = n2, to = n1, findSuccessors)
    }
  }

  def getSuccessorsRecursive[T](method: T, findSuccessors: (T) => List[T]): List[T] = {
    @tailrec
    def go(xs: List[T], result: List[T]): List[T] = {
      xs match {
        case Nil =>
          result
        case h :: t if result.contains(h) =>
          go(t, result)
        case h :: t =>
          val unseen = for {
            successor <- findSuccessors(h) if !result.contains(successor)
          } yield successor
          go(t ::: unseen, h :: result)
      }
    }

    go(findSuccessors(method), Nil)
  }

}
