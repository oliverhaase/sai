package sai.vm

import scala.annotation.tailrec

case class OpStack(elements: List[Slot] = Nil) {

  def push(element: Slot, i: Int): OpStack = copy(List.fill(i)(element) ::: elements)

  def depth: Int = elements.size

  def push(element: Slot): OpStack = copy(element :: elements)

  def peek: Slot = elements.head

  def pop: OpStack = copy(elements.tail)

  @tailrec
  final def pop(n: Int): OpStack = n match {
    case 0 => this
    case x => pop.pop(x - 1)
  }

}
