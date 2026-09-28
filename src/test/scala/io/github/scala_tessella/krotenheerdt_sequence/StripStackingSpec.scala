package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SymbolCatalog
import StripStacking.*
import StripStacking.Letter.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The strip-stacking fixtures: the single-letter words are the classical prismatic honeycombs with their
  * known minimal symbol sizes, the word Kw0 Ku0 C C is the 120-chamber k = 4 class of the 108-world with its
  * four species (a class of the k = 4 census), and the word algebra is invariant under the congruences it
  * claims.
  */
class StripStackingSpec extends AnyFlatSpec with Matchers:

  private def symbolOf0(word: Vector[Letter]): S =
    val (cells, per) = build(word)
    val fl           = flags(cells, per).getOrElse(fail(s"word ${word.map(_.label)}: faces not shared pairwise"))
    minimalImage(S(fl.s, fl.m01, fl.m23, fl.cell))

  "the cubic word" should "be the one-chamber cubic honeycomb" in {
    symbolOf0(Vector(C)).size shouldBe 1
  }

  "single strip letters" should "give the triangular, kagome and elongated triangular prismatic honeycombs" in {
    symbolOf0(Vector(Tu)).size shouldBe 3
    // a hexagon's position is relative to its own row's grid, and a row pair shifts the grid by one unit: Ku0
    // repeated stacks hexagon over rhombus — the kagome prismatic honeycomb — and Ku0 Ku1 hexagon over hexagon
    val kagome  = symbolOf0(Vector(Ku0))
    kagome.size shouldBe 6
    kagome.orbitsOf(Vector(1, 2, 3)).max + 1 shouldBe 1
    val stacked = symbolOf0(Vector(Ku0, Ku1))
    stacked.size shouldBe 18
    speciesOf(stacked).toSet shouldBe
      Set(Set(19), Set(21)) // {p3:4 p6:4}#1 and #3: the lift of [3.3.6.6; 3.6.3.6]
    symbolOf0(Vector(C, Tu)).size shouldBe 15
  }

  "the 108-world word" should "be the 120-chamber k = 4 class with its four species" in {
    val x = symbolOf0(Vector(Kw0, Ku0, C, C))
    x.size shouldBe 120
    x.orbitsOf(Vector(1, 2, 3)).max + 1 shouldBe 4
    speciesOf(x).map(_.toVector.sorted).toSet shouldBe Set(Vector(16), Vector(18), Vector(20), Vector(21))
    SymbolCatalog.valid(x.toSym) shouldBe true
  }

  "every word up to length 2" should "build a valid symbol whose minimal image is minimal" in {
    val ws = for a <- Letter.values.toVector; b <- Letter.values.toVector yield Vector(a, b)
    for w <- Letter.values.toVector.map(Vector(_)) ++ ws.map(canonicalWord).distinct do
      val x = symbolOf0(w)
      withClue(w.map(_.label).mkString(" ")) {
        SymbolCatalog.valid(x.toSym) shouldBe true
        congruence(x) shouldBe None
      }
  }

  "the translation pre-quotient" should "not change the minimal symbol" in {
    for w <- Vector(Vector(Ku0), Vector(C, Tu), Vector(Kw0, Ku0, C, C), Vector(Tu, Tu, Kw0, Kw0)) do
      val fast = symbolOf(w).getOrElse(fail("symbolOf"))
      canonicalKey(fast) shouldBe canonicalKey(symbolOf0(w))
  }

  "the general layer model" should "contain the kagome letters and the hexagonal prismatic honeycomb" in {
    import Layer.*
    val kagome    = symbolOfLayers(Vector(Tri(true, Set(0), 2), Tri(true, Set.empty, 1))).get
    canonicalKey(kagome) shouldBe canonicalKey(symbolOf(Vector(Ku0)).get)
    // the 6³ block: a hexagon every third position in every row pair, offset by one from pair to pair
    val hexPrisms =
      symbolOfLayers(Vector(Tri(true, Set(0), 3), Tri(true, Set(1), 3), Tri(true, Set(2), 3))).get
    hexPrisms.orbitsOf(Vector(1, 2, 3)).max + 1 shouldBe 1
    speciesOf(hexPrisms) shouldBe Vector(Set(13))                                          // {p6:6}#1
    hexPrisms.cell.toSet shouldBe Set(10)
    symbolOfLayers(Vector(Tri(true, Set(0), 2), Cubic)) shouldBe None                      // a merge needs a same-axis row above
    symbolOfLayers(Vector(Tri(true, Set(0, 1), 2), Tri(true, Set.empty, 1))) shouldBe None // hexagons overlap
  }

  "the interface species oracle" should "read the 108-world species off a four-layer window" in {
    import Layer.*
    val sp =
      interfaceSpecies(Vector(Cubic, Cubic, Tri(true, Set(0), 2), Tri(true, Set.empty, 1), Cubic, Cubic))
    sp(0) shouldBe Set(18) // cube on cube: {cube:8}#1
    sp(1) shouldBe Set(16) // cube on kagome row: {cube:4 p3:2 p6:2}#1
    sp(2) shouldBe Set(21) // the kagome row's mid-vertices: {p3:4 p6:4}#3
    sp(3) shouldBe Set(16)
  }

  "layer words as text" should "round-trip" in {
    import Layer.*
    val w = Vector(
      Cubic,
      Tri(true, Set(1, 3), 4),
      Tri(true, Set.empty, 1),
      Tri(false, Set(0), 3),
      Tri(false, Set.empty, 1)
    )
    parseWord(showLayers(w)) shouldBe w
    showLayers(w) shouldBe "C Tu{1,3/4} Tu Tw{0/3} Tw"
  }

  "the letter group" should "have order 8" in {
    letterGroup.size shouldBe 8
    letterGroup.map(g => Letter.values.toVector.map(g)).distinct.size shouldBe 8
  }

  "canonical words" should "be invariant under rotation, reversal, axis swap and phase flips" in {
    val w = Vector(Kw0, Ku1, C, Tu, C)
    canonicalWord(w.reverse) shouldBe canonicalWord(w)
    canonicalWord(w.drop(2) ++ w.take(2)) shouldBe canonicalWord(w)
    canonicalWord(Vector(Ku0, Kw1, C, Tw, C)) shouldBe canonicalWord(w)
    canonicalWord(Vector(Kw0, Ku0, C, Tu, C)) shouldBe canonicalWord(w)
    canonicalWord(Vector(Kw1, Ku1, C, Tu, C)) shouldBe canonicalWord(w)
  }
