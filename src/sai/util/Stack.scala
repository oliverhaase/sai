package sai.util

import scala.annotation.tailrec

case class Stack[T](private val elements: List[T]) {

  def depth: Int = elements.size

  def push(element: T): Stack[T] = copy(element :: elements)

  def dup: Stack[T] = push(peek)

  def peek: T = elements.head

  def pop: (T, Stack[T]) = {
    val (poppedElement :: Nil, newStack) = pop(1)
    (poppedElement, newStack)
  }

  def pop(n: Int): (List[T], Stack[T]) = {
    @tailrec
    def popRec(x: Int, elems: List[T], poppedElements: List[T]): (List[T], Stack[T]) = {
      if (x == 0) (poppedElements, Stack(elems))
      else popRec(x - 1, elems.tail, poppedElements :+ elems.head)
    }
    popRec(n, elements, Nil)
  }
}

object Stack {
  def apply[T](): Stack[T] = Stack(Nil)
}
