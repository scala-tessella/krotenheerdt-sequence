package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import HexagonNecklaces.*
import HexagonNecklaces.Layer.*

/** The hexagon-world necklaces (the paper's certified fact C11): H_k = 1, 3, 6, 4 and none from k = 5, the
  * 1-uniform class being the quarter-cubic honeycomb of the 28. Anchored on Andreini's 13′ (the mirrored
  * stacking of quarter-cubic slabs, the word S1 S0) and on the thirteen hexagon-world classes of the census
  * rows 2 to 4, whose species sets the necklaces reproduce one for one. The model was matched class by class
  * against the geometry of the census honeycombs before this spec was written (every census class's stretch
  * of layers occurs in exactly one necklace, with its species).
  */
class HexagonNecklacesSpec extends AnyFlatSpec with Matchers:

  "the junction dictionary" should "give the four hexagon-world species" in:
    junction(K, K) shouldBe 21
    junction(S0, K) shouldBe 22
    junction(K, S1) shouldBe 22
    // S0 carries its upper tetrahedra on class 1, S1 its lower ones on class 1: they meet face to face
    junction(S0, S1) shouldBe 23
    junction(S0, S0) shouldBe 24
    // the rule: the lower slab's upper class (1 − a) equal to the upper slab's lower class b gives 23
    for a <- List(S0, S1); b <- List(S0, S1) do
      val upper = if a == S0 then 1 else 0
      val lower = if b == S0 then 0 else 1
      junction(a, b) shouldBe (if upper == lower then 23 else 24)

  "Andreini's 13′" should "be the word S1 S0, 2-uniform on the two quarter-cubic stars" in:
    val c = classes.find(_.word == canonical(Vector(S1, S0))).get
    c.species shouldBe Vector(23, 24)

  "the hexagon-world necklaces" should "number 3, 6, 4 for k = 2, 3, 4 and none beyond" in:
    classes.groupMapReduce(_.k)(_ => 1)(_ + _) shouldBe Map(2 -> 3, 3 -> 6, 4 -> 4)
    // the census: the species sets of its thirteen hexagon-world classes
    classes.map(_.species).groupMapReduce(identity)(_ => 1)(_ + _) shouldBe Map(
      Vector(22, 24)         -> 2,
      Vector(23, 24)         -> 1,
      Vector(21, 22, 24)     -> 4,
      Vector(22, 23, 24)     -> 2,
      Vector(21, 22, 23, 24) -> 4
    )

  "the quarter-cubic word" should "be the degenerate class, two level orbits of one species" in:
    orbits(Vector(S0)).map(_.toVector) should contain theSameElementsAs Vector(Vector(24), Vector(24))

  "an over-sweep beyond the bound" should "find nothing new" in:
    enumerate(10).map(_.word).toSet shouldBe classes.map(_.word).toSet
