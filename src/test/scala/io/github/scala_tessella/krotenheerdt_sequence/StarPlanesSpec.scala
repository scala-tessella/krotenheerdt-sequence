package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.SpeciesCorona
import StarPlanes.*

/** The junction planes of the species stars: the facts F1–F3 of the paper behind its theorems on the
  * hexagon-world necklace and on the separation of octet cells from cubes and hexagonal prisms. F2 lists, per
  * star, every junction plane and the faces of the star it contains: the propagation of the two theorems
  * starts at a star whose plane is unique (S|K, the face-sharing S|S star; the elongated and the
  * close-packing octet stars) and needs, at the other stars, that every face lies in one of the planes.
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

  private def kagomePlanes(i: Int)                = junctionPlanes(i).filter(s => cyclic(s.pattern) == kagome)
  private def inPlane(s: Split, f: Face)          = s.faces.exists(sameFace(_, f))
  private def cells(fc: (Face, Vector[Corner]))   = fc._2.map(_.cell).sorted
  private def basePlaneOfPrisms(i: Int, s: Split) =
    starOf(i).filter(c => c.cell.startsWith("p")).forall(c => prismAxis(c).exists(sameLine(_, s.normal)))

  "F2, the planes of the hexagon-world stars" should
    "be one for S|K and K|K (the prisms' base plane), one for the face-sharing star through its two shared triangles, three for the other covering every face" in {
      val i1            = idx("{tet:2 truncTet:6}#1")
      val i2            = idx("{tet:2 truncTet:6}#2")
      def faces(i: Int) = starFaces(starOf(i))
      def tt(i: Int)    = faces(i).filter(cells(_) == Vector("tet", "tet"))
      def xx3(i: Int)   = faces(i).filter(fc => fc._1.sides == 3 && cells(fc) == Vector("truncTet", "truncTet"))
      // the index 1 is the star whose two tetrahedra share a face; both stars have six hexagons and six triangles
      tt(i1).size shouldBe 1
      tt(i2) shouldBe empty
      for i <- Vector(i1, i2) do
        faces(i).size shouldBe 12
        faces(i).count(_._1.sides == 6) shouldBe 6
        faces(i).forall(_._2.size == 2) shouldBe true
      // the face-sharing star: one plane, containing its T|T triangle and its single X|X triangle
      xx3(i1).size shouldBe 1
      xx3(i2) shouldBe empty
      val p1            = kagomePlanes(i1)
      p1.size shouldBe 1
      inPlane(p1.head, tt(i1).head._1) shouldBe true
      inPlane(p1.head, xx3(i1).head._1) shouldBe true
      // the other star: three planes, every hexagon and triangle of the star in exactly one
      val p2            = kagomePlanes(i2)
      p2.size shouldBe 3
      for (f, _) <- faces(i2) do withClue(f)(p2.count(inPlane(_, f)) shouldBe 1)
      // S|K and K|K: one plane, the base plane of the prisms
      for lab <- Vector("{p3:4 p6:4}#3", "{tet:1 truncTet:3 p3:2 p6:2}#1") do
        val ps = kagomePlanes(idx(lab))
        withClue(lab)(ps.size shouldBe 1)
        withClue(lab)(basePlaneOfPrisms(idx(lab), ps.head) shouldBe true)
    }

  "F2, the planes of the octet stars" should
    "be one for the elongated star (the prisms' base plane), one for the close-packing star through its shared triangles, four for the cuboctahedral star covering every triangle" in {
      val Vector(a, b)   = Vector("{tet:8 oct:6}#1", "{tet:8 oct:6}#2").map(idx)
      def paired(i: Int) = starFaces(starOf(i)).filter(fc => cells(fc).distinct.size == 1) // T|T or O|O
      val (hcp, fcc)     = if paired(a).nonEmpty then (a, b) else (b, a)
      paired(fcc) shouldBe empty
      paired(hcp).size shouldBe 6 // three triangles shared by two tetrahedra, three by two octahedra
      val ph             = junctionPlanes(hcp)
      ph.size shouldBe 1
      paired(hcp).forall((f, _) => inPlane(ph.head, f)) shouldBe true
      val pf             = junctionPlanes(fcc)
      pf.size shouldBe 4
      val ff             = starFaces(starOf(fcc))
      ff.size shouldBe 24
      for (f, _) <- ff do withClue(f)(pf.count(inPlane(_, f)) shouldBe 1)
      val el             = idx("{tet:4 oct:3 p3:6}#1")
      val pe             = junctionPlanes(el)
      pe.size shouldBe 1
      basePlaneOfPrisms(el, pe.head) shouldBe true
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
