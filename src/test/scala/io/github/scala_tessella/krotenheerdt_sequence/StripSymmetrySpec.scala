package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StripStacking.*
import StripStacking.Layer.*
import StripSymmetry.*

/** The symmetries of stacking words: the layer group, the reversal (a mirror image keeps the symbol), the
  * junction maps of a word's symmetries and the orbit lower bound.
  */
class StripSymmetrySpec extends AnyFlatSpec with Matchers:

  private def key(w: Vector[Layer]): Vector[Int] =
    canonicalKey(symbolOfLayers(w).getOrElse(fail(s"inconsistent word ${showLayers(w)}")))

  private def orbits(w: Vector[Layer]): Int = symbolOfLayers(w).get.orbitsOf(Vector(1, 2, 3)).max + 1

  "the in-plane group" should "have 32·period² elements, one of them the identity" in {
    inPlaneGroup(2).size shouldBe 128
    inPlaneGroup(3).size shouldBe 288
    inPlaneGroup(2).count(_.isIdentity) shouldBe 1
  }

  "the image of a segment" should "shift, swap and reflect the hexagon residues with the grid drift" in {
    val shiftU = InPlane(false, false, false, 2, 0)
    val swap   = InPlane(true, false, false, 0, 0)
    val reflU  = InPlane(false, true, false, 0, 0)
    apply(shiftU, parseWord("Tu{0/2} Tu Tw{0/2} Tw")) shouldBe Some(parseWord("Tu{1/2} Tu Tw{0/2} Tw"))
    apply(swap, parseWord("Tu{0/2} Tu C")) shouldBe Some(parseWord("Tw{0/2} Tw C"))
    // a reflection: residue m of the row of rank ρ goes to −m − ρ − 1
    apply(reflU, parseWord("Tu{0/3} Tu Tu{0/3} Tu")) shouldBe Some(parseWord("Tu{2/3} Tu Tu{0/3} Tu"))
    apply(InPlane(false, true, false, 1, 0), parseWord("Tu{0/2} Tu")) shouldBe
      None // a half-unit off the grid
    // placed after itself the segment's rows start at the offsets reached, and the grid has drifted by one
    // unit: the identity moves the residues, a shift by one unit restores them (the plain repetition)
    image(InPlane(false, false, false, 0, 0), parseWord("Tu{0/2} Tu Tw"), (0, 0), (2, 1)) shouldBe
      Some(parseWord("Tu{1/2} Tu Tw"))
    image(shiftU, parseWord("Tu{0/2} Tu Tw"), (0, 0), (2, 1)) shouldBe Some(parseWord("Tu{0/2} Tu Tw"))
  }

  it should "give a word with the same symbol under every reflection (a mirror image)" in {
    for txt <- Vector("C Tu{0/2} Tu Tw", "Tu Tw{0/3} Tw{1/3} Tw{2/3} Tw", "C C Tu{0/2} Tu Tw Tu{1/2} Tu") do
      val w = parseWord(txt)
      for g <- Vector(
                 InPlane(false, true, false, 0, 0),
                 InPlane(false, false, true, 0, 0),
                 InPlane(true, true, true, 0, 0)
               )
      do
        withClue(s"$txt under $g")(apply(g, w).map(key) shouldBe Some(key(w)))
  }

  "the chains" should "stop when the chain closes and respect the length bound" in {
    val s = parseWord("C Tu{0/2} Tu")
    chains(s, InPlane(false, false, false, 0, 0), 12).map(showLayers) shouldBe
      Vector("C Tu{0/2} Tu C Tu{1/2} Tu")
    chains(s, InPlane(false, false, false, 2, 0), 12) shouldBe
      empty // a shift by one unit: the plain repetition
    chains(s, InPlane(true, false, false, 0, 0), 12).size shouldBe 3
    chains(s, InPlane(true, false, false, 0, 0), 6).size shouldBe 1
  }

  "the reversal" should "move each hexagon pair to its new lower row" in {
    def hexRows(w: Vector[Layer]) = w.map { case Tri(_, hx, _) => hx.nonEmpty; case _ => false }
    hexRows(reverseLayers(parseWord("C Tu{0/2} Tu"))) shouldBe Vector(true, false, false)
    hexRows(reverseLayers(parseWord("Tu Tu{0/3} Tu{1/3}"))) shouldBe Vector(true, false, true)
    reverseLayers(reverseLayers(parseWord("C Tu{0/2} Tu Tw"))).size shouldBe 4
  }

  it should "give a word with the same symbol as the original (a mirror image)" in {
    for
      txt <- Vector(
               "C Tu{0/2} Tu",
               "C Tu{0/2} Tu Tw",
               "Tu Tu{0/3} Tu{1/3}",
               "C C Tu{0/2} Tu Tw Tu{1/2} Tu",
               "Tu Tu Tw{1/2} Tw C Tu{0/2} Tu Tw Tw Tu{1/2} Tu C Tw{0/2} Tw",
               "Tu Tw{0/3} Tw{1/3} Tw{2/3} Tw",
               "Tu Tw{0/4} Tw Tu Tw{3/4} Tw"
             )
    do
      val w = parseWord(txt)
      withClue(txt)(key(reverseLayers(w)) shouldBe key(w))
  }

  "the junction maps" should "read the reflection with axis swap of the 108-world word" in {
    val w    = parseWord("Tw{0/2} Tw Tu{0/2} Tu C C")
    val syms = wordSymmetries(w, inPlaneGroup(2))
    syms should contain(JunctionMap(1, 0, 6))
    syms.exists(_.a == -1) shouldBe true
    junctionOrbits(6, syms).distinct.size shouldBe 4
  }

  it should "read the swap-translation of a doubled word and the reflection of a plain row" in {
    val w = parseWord("C C Tu Tw Tw C C Tw Tu Tu")
    wordSymmetries(w, inPlaneGroup(2)) should contain(JunctionMap(1, -5, 10))
    junctionOrbits(2, wordSymmetries(parseWord("Tu Tw"), inPlaneGroup(2))).distinct.size shouldBe 1
  }

  "the orbit lower bound" should
    "equal the vertex orbits on Krötenheerdt words and stay below them otherwise" in {
      def bound(w: Vector[Layer]): Int =
        val group = inPlaneGroup(w.collect { case Tri(_, hx, p) if hx.nonEmpty => p }.foldLeft(1)(lcm).max(2))
        val sp    = w.indices.map { j =>
          val win = Vector(rotate(w, j - 2).head, rotate(w, j - 1).head, w(j), rotate(w, j + 1).head)
          j -> interfaceSpecies(win).getOrElse(1, Set(-1))
        }.toMap
        orbitSpecies(w, group, sp).map(_.map(_.size).sum).getOrElse(-1)
      for txt <- Vector("Tu", "Tu Tw", "C Tu", "Tw{0/2} Tw Tu{0/2} Tu C C", "C C Tu{0/2} Tu Tw Tu{1/2} Tu") do
        val w = parseWord(txt)
        withClue(txt)(bound(w) shouldBe orbits(w))
      val screw                        =
        parseWord("Tu Tw{0/3} Tw{1/3} Tw{2/3} Tw") // the screw-stacked 6³ block: six orbits, bound four
      bound(screw) should be < orbits(screw)
    }

  "the canonical segment" should "identify a segment with its images and keep it apart from a rotation" in {
    val g = inPlaneGroup(2)
    val s = parseWord("C Tu{0/2} Tu Tw")
    canonicalSegment(apply(InPlane(true, true, false, 2, 0), s).get, g) shouldBe canonicalSegment(s, g)
    canonicalSegment(rotate(s, 1), g) should not be canonicalSegment(s, g)
  }

  "the canonical label" should "be invariant under rotation, reversal and the layer group" in {
    val w = parseWord("C C Tu{0/2} Tu Tw Tu{1/2} Tu")
    val g = inPlaneGroup(2)
    canonicalLayers(rotate(w, 3), g) shouldBe canonicalLayers(w, g)
    canonicalLayers(reverseLayers(w), g) shouldBe canonicalLayers(w, g)
    canonicalLayers(apply(InPlane(true, true, false, 2, 2), w).get, g) shouldBe canonicalLayers(w, g)
  }
