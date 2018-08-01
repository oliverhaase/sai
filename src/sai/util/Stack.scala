package sai.util

import scala.annotation.tailrec

case class Stack[T](private val elements: List[T]) {

  def push(element: T, i: Int): Stack[T] = copy(List.fill(i)(element) ::: elements)

  def depth: Int = elements.size

  def push(element: T): Stack[T] = copy(element :: elements)

  def dup: Stack[T] = push(peek)

  def peek: T = elements.head

  def pop: Stack[T] = copy(elements.tail)

  @tailrec
  final def pop(n: Int): Stack[T] = n match {
    case 0 => this
    case x => pop.pop(x - 1)
  }
}

object Stack {
  def apply[T](): Stack[T] = Stack(Nil)
}
