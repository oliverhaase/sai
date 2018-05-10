package scala

import cg.EscapeStates
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import cg.EscapeStates._

class EscapeStatesTest extends FlatSpec with Matchers {

  "An EscapeState" should "have an ordering" in {
    GlobalEscape < ArgEscape && ArgEscape < NoEscape shouldBe true
  }

  it should "merge two escape states with each other" in {
    EscapeStates.merge(NoEscape, NoEscape) shouldBe NoEscape
    EscapeStates.merge(NoEscape, ArgEscape) shouldBe ArgEscape
    EscapeStates.merge(NoEscape, GlobalEscape) shouldBe GlobalEscape

    EscapeStates.merge(ArgEscape, NoEscape) shouldBe ArgEscape
    EscapeStates.merge(ArgEscape, ArgEscape) shouldBe ArgEscape
    EscapeStates.merge(ArgEscape, GlobalEscape) shouldBe GlobalEscape

    EscapeStates.merge(GlobalEscape, NoEscape) shouldBe GlobalEscape
    EscapeStates.merge(GlobalEscape, ArgEscape) shouldBe GlobalEscape
    EscapeStates.merge(GlobalEscape, GlobalEscape) shouldBe GlobalEscape
  }

  it should "have a string representation" in {
    GlobalEscape.toString shouldBe "⊥"
    NoEscape.toString shouldBe "⊤"
  }

}
