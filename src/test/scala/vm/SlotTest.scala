package scala.vm

import cg.LocalReferenceNode
import org.apache.bcel.generic.ObjectType
import org.scalatest.{FlatSpec, Matchers}
import sai.vm.Reference.Null
import sai.vm.{DontCare, MultivalueSlot, Reference}

class SlotTest extends FlatSpec with Matchers {

  "A DontCare slot" should "merge with another DontCare slot" in {
    val result = DontCare.merge(DontCare)
    result shouldBe DontCare
  }

  "A Null slot" should "merge with another slot" in {
    val result = Null.merge(DontCare)
    result shouldBe MultivalueSlot(Null, DontCare)
  }

  "A Reference slot" should "merge with another Reference slot" in {
    val ref1 = Reference(new ObjectType("my.class1"), LocalReferenceNode("L1"))
    val ref2 = Reference(new ObjectType("my.class2"), LocalReferenceNode("L2"))
    val result = ref1.merge(ref2)
    result shouldBe MultivalueSlot(ref1, ref2)
  }

  "A Multivalue slot" should "merge with another mutlivalue slot" in {
    val ref = Reference(new ObjectType("my.class2"), LocalReferenceNode("L2"))
    val slot1 = MultivalueSlot(Null, DontCare)
    val slot2 = MultivalueSlot(DontCare, ref)
    val result = slot1.merge(slot2)
    result shouldBe MultivalueSlot(ref, DontCare, Null)
  }

  "A Multivalue slot" should "merge with another slot" in {
    val result = MultivalueSlot(Null).merge(DontCare)
    val expected = MultivalueSlot(Null, DontCare)
    result shouldBe expected
  }

  "A slot" should "merge with a multivalue slot" in {
    val result = DontCare.merge(MultivalueSlot(Null))
    result shouldBe MultivalueSlot(DontCare, Null)
  }
}
