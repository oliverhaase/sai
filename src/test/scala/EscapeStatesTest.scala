package scala

import ea.{ArgEscape, GlobalEscape, NoEscape}
import org.scalatest.{FlatSpec, Matchers}

class EscapeStatesTest extends FlatSpec with Matchers {

  "An EscapeState" should "have an ordering" in {
    GlobalEscape < ArgEscape && ArgEscape < NoEscape shouldBe true
  }

  it should "merge two escape states with each other" in {
    NoEscape merge NoEscape shouldBe NoEscape
    NoEscape merge ArgEscape shouldBe ArgEscape
    NoEscape merge GlobalEscape shouldBe GlobalEscape

    ArgEscape merge NoEscape shouldBe ArgEscape
    ArgEscape merge ArgEscape shouldBe ArgEscape
    ArgEscape merge GlobalEscape shouldBe GlobalEscape

    GlobalEscape merge NoEscape shouldBe GlobalEscape
    GlobalEscape merge ArgEscape shouldBe GlobalEscape
    GlobalEscape merge GlobalEscape shouldBe GlobalEscape
  }

  it should "have a string representation" in {
    GlobalEscape.toString shouldBe "⊥"
    ArgEscape.toString shouldBe "-"
    NoEscape.toString shouldBe "⊤"
  }

}
