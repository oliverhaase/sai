package implicits

class MutableSetExtensions[T](val set: scala.collection.mutable.Set[T]) {
  def removeArbitrary(): T = {
    val head = set.head
    set -= head
    head
  }
}

object MutableSetExtensions {
  implicit def convert[T](set: scala.collection.mutable.Set[T]) = new MutableSetExtensions[T](set)
}
