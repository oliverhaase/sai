package cg

object EscapeStates extends Enumeration {
  type EscapeState = Value

  val GlobalEscape: EscapeState = Value(0, "⊥")
  val ArgEscape: EscapeState = Value(1)
  val NoEscape: EscapeState = Value(2, "⊤")

  def merge(es: EscapeState, es2: EscapeState): EscapeState = (es, es2) match {
    case (`es`, `es`) => es
    case (`es`, NoEscape) => es
    case (`es`, GlobalEscape) => GlobalEscape
    case _ => merge(es2, es)
  }

}