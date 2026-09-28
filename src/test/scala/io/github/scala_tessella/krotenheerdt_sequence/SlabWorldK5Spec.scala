package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** k = 5, SLAB SIDE: THE SLAB WORLD DIES AT k = 5. The complete SlabNecklaces census (the necklace
  * enumeration under the proven m ≤ 2k quasi-period bound, `SlabNecklacesSpec`) carries NO class with five
  * distinct species: the slab Krötenheerdt sequence over the alphabet {c, e, h, p, x} is EXACTLY 6, 20, 24,
  * 16, 0, 0, … — all five letters are in use across classes, but no single class reaches five. Consequently
  * the k = 5 known-answer floor is the 15 prismatic lifts alone (`PrismaticLiftsK5Spec`): N₅(3D) ≥ 15 with no
  * slab contribution. Fast tier (the census is cheap).
  */
class SlabWorldK5Spec extends AnyFlatSpec with Matchers:

  "the slab necklace census" should "carry NO five-species class: the slab world dies at k = 5" in:
    KSlabId.classesOf(5) shouldBe empty
    // the complete slab Krötenheerdt distribution, pinned
    SlabNecklaces.classes.groupBy(_.k).view.mapValues(_.size).toMap shouldBe
      Map(1 -> 6, 2 -> 20, 3 -> 24, 4 -> 16)
    // every letter of the alphabet participates in some class — the death at 5 is combinatorial,
    // not a missing letter
    SlabNecklaces.classes.flatMap(nc => KSlabId.kSetOf(nc)._1).distinct.sorted shouldBe
      Vector('c', 'e', 'h', 'p', 'x')
