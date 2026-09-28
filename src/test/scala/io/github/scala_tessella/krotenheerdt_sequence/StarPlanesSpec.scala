package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.SpeciesCorona
import StarPlanes.*

/** The junction planes of the species stars: the facts F1–F3 of the paper behind its theorems on the
  * hexagon-world necklace and on the separation of octet cells from cubes and hexagonal prisms).
  */
class StarPlanesSpec extends AnyFlatSpec with Matchers:

  private val labels: Vector[String]  =
    io.github.scala_tessella.research_core.SpeciesEnumerator.species.indices.toVector.map(SpeciesCorona.label)
  private def idx(label: String): Int =
    labels.indexOf(label) match
      case -1 => fail(s"no species $label")
      case i  => i

  private val kagome                 = cyclic(Vector(3, 6, 3, 6))
  private def cubicFamily(l: String) =
    Vector("co:", "rco:", "tco:", "truncOct:", "truncCube:").exists(l.contains)

  "the faces of a corner" should "be the edge pairs at a polygon angle of the cell" in {
    val tet = starOf(idx("{tet:8 oct:6}#1")).find(_.cell == "tet").get
    cornerFaces(tet).map(_.sides) shouldBe Vector(3, 3, 3)
    val oct = starOf(idx("{tet:8 oct:6}#1")).find(_.cell == "oct").get
    cornerFaces(oct).map(_.sides) shouldBe Vector(3, 3, 3, 3) // the two 90° diagonals are not faces
    val p6 = starOf(idx("{p6:6}#1")).head
    cornerFaces(p6).map(_.sides).sorted shouldBe Vector(4, 4, 6)
  }

  "F1" should "name the species with a truncated tetrahedron and those with an octet cell" in {
    speciesWith("truncTet").map(SpeciesCorona.label).filterNot(cubicFamily).toSet shouldBe
      Set("{tet:1 truncTet:3 p3:2 p6:2}#1", "{tet:2 truncTet:6}#1", "{tet:2 truncTet:6}#2")
    speciesWith("truncTet").map(SpeciesCorona.label).filter(cubicFamily).size shouldBe 2 // the cantic ones
    val octet = (speciesWith("tet") ++ speciesWith("oct")).distinct.map(SpeciesCorona.label)
      .filterNot(l => l.contains("truncTet") || cubicFamily(l))
    octet.toSet shouldBe Set("{tet:4 oct:3 p3:6}#1", "{tet:8 oct:6}#1", "{tet:8 oct:6}#2")
    // no species outside the cubic family carries an octet cell together with a cube or a hexagonal prism
    for l <- (speciesWith("tet") ++ speciesWith("oct")).distinct.map(SpeciesCorona.label) if !cubicFamily(l)
    do
      withClue(l)(l.contains("cube:") shouldBe false)
      withClue(l)(l.contains("p6:") && !l.contains("truncTet") shouldBe false)
  }

  "F2" should "give the kagome junction planes of species 21 to 24 with their sides" in {
    def kagomePlanes(i: Int) = junctionPlanes(i).filter(s => cyclic(s.pattern) == kagome)
    val all                  = junctionPlanes(idx("{p3:4 p6:4}#3"))
    all.size shouldBe 3 // the kagome plane and the two vertical square-tiled planes over the kagome lines
    all.map(s => cyclic(s.pattern)).count(_ == cyclic(Vector(4, 4, 4, 4))) shouldBe 2
    val kk = kagomePlanes(idx("{p3:4 p6:4}#3"))
    kk.size shouldBe 1
    kk.head.below shouldBe Side(Map("p3" -> 2, "p6" -> 2), 4, 0)
    kk.head.above shouldBe Side(Map("p3" -> 2, "p6" -> 2), 4, 0)
    val sk = kagomePlanes(idx("{tet:1 truncTet:3 p3:2 p6:2}#1"))
    sk.size shouldBe 1
    Set(sk.head.below, sk.head.above) shouldBe
      Set(Side(Map("p3" -> 2, "p6" -> 2), 4, 0), Side(Map("tet" -> 1, "truncTet" -> 3), 0, 0))
    for lab <- Vector("{tet:2 truncTet:6}#1", "{tet:2 truncTet:6}#2") do
      val ss = junctionPlanes(idx(lab))
      withClue(lab)(ss should not be empty)
      for s <- ss do
        withClue(lab)(cyclic(s.pattern) shouldBe kagome)
        withClue(lab)(s.below shouldBe Side(Map("tet" -> 1, "truncTet" -> 3), 0, 0))
        withClue(lab)(s.above shouldBe Side(Map("tet" -> 1, "truncTet" -> 3), 0, 0))
    // one of the two S|S stars is the quarter-cubic vertex: three of its four tetrahedral planes are kagome
    // junction planes, for the fourth the vertex is a tetrahedron apex inside the slab; the gyrated one keeps one
    Vector("{tet:2 truncTet:6}#1", "{tet:2 truncTet:6}#2").map(l => kagomePlanes(idx(l)).size).sorted shouldBe
      Vector(1, 3)
  }

  it should "give the triangle-tiled junction planes of the octet species" in {
    val octet = Side(Map("tet" -> 4, "oct" -> 3), 0, 0)
    val el    = junctionPlanes(idx("{tet:4 oct:3 p3:6}#1"))
    el.size shouldBe 1
    el.head.pattern shouldBe Vector(3, 3, 3, 3, 3, 3)
    Set(el.head.below, el.head.above) shouldBe Set(octet, Side(Map("p3" -> 6), 6, 0))
    for lab <- Vector("{tet:8 oct:6}#1", "{tet:8 oct:6}#2") do
      val ss = junctionPlanes(idx(lab))
      withClue(lab)(ss should not be empty)
      for s <- ss do
        withClue(lab)(s.pattern shouldBe Vector(3, 3, 3, 3, 3, 3))
        withClue(lab)(s.below shouldBe octet)
        withClue(lab)(s.above shouldBe octet)
  }

  "F3" should "leave only species 21 and 22 with a kagome plane and a K side" in {
    val withBoth = labels.indices.filter { i =>
      val l = SpeciesCorona.label(i); l.contains("p3:") && l.contains("p6:")
    }
    val kSide    = Side(Map("p3" -> 2, "p6" -> 2), 4, 0)
    val hits     = withBoth.filter(i =>
      junctionPlanes(i).exists(s => cyclic(s.pattern) == kagome && (s.below == kSide || s.above == kSide))
    )
    hits.map(SpeciesCorona.label).toSet shouldBe Set("{p3:4 p6:4}#3", "{tet:1 truncTet:3 p3:2 p6:2}#1")
  }
