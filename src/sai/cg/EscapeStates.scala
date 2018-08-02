package cg

sealed abstract class EscapeState(private val id: Int, private val s: String) extends Ordered[EscapeState] {

  def merge(other: EscapeState): EscapeState = if (this < other) this else other

  override def compare(other: EscapeState): Int = id.compareTo(other.id)

  override def toString: String = s
}

case object GlobalEscape extends EscapeState(0, "⊥")
case object ArgEscape extends EscapeState(1, "-")
case object NoEscape extends EscapeState(2, "⊤")
