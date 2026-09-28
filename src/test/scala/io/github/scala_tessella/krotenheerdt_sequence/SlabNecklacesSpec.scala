package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.HoneycombAlphabet
import io.github.scala_tessella.research_core.SpeciesEnumerator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SlabNecklaces.*
import SlabNecklaces.Species.*

/** The Krötenheerdt classification of the {tet, oct, P3} slab alphabet. The finite layers of the structure
  * theorem are certified against the edge-figure catalogue and the species table; the necklace enumeration is
  * anchored on the Barlow stackings, the 28 (the six 1-uniform classes, counted by the completeness audit),
  * hand-verified words, and the quasi-period bound with an over-sweep.
  */
class SlabNecklacesSpec extends AnyFlatSpec with Matchers:

  import HoneycombAlphabet.CellType

  val slabCells: Set[CellType] = Set(CellType.Tet, CellType.Oct, CellType.P3)

  "the edge-figure catalogue restricted to {tet, oct, p3}" should
    "contain exactly the six slab-world figures" in:
      val figs = HoneycombAlphabet.restrictedTo(slabCells).map(_.show)
      figs should contain theSameElementsAs Vector(
        "[tet(3·3) tet(3·3) oct(3·3) oct(3·3)]",            // h-plane edge (non-alternating octet)
        "[tet(3·3) oct(3·3) tet(3·3) oct(3·3)]",            // c-plane edge (alternating octet)
        "[tet(3·3) oct(3·3) p3(3·4) p3(4·3)]",              // elongation edge (octet·prism junction)
        "[p3(3·4) p3(4·3) p3(3·4) p3(4·3)]",                // square-plane edge (four base corners)
        "[p3(4·4) p3(4·4) p3(4·4) p3(4·3) p3(3·4)]",        // mixed-axis edge (three lateral + two base)
        "[p3(4·4) p3(4·4) p3(4·4) p3(4·4) p3(4·4) p3(4·4)]" // the all-lateral column ring
      )

  "the species table restricted to {tet, oct, p3}" should "be exactly the five slab species" in:
    val slab  = SpeciesEnumerator.species.filter(_.counts.keySet.subsetOf(slabCells))
    slab should have size 5
    slab.map(_.showSupport).groupMapReduce(identity)(_ => 1)(_ + _) shouldBe
      Map("{tet:8 oct:6}" -> 2, "{tet:4 oct:3 p3:6}" -> 1, "{p3:12}" -> 2)
    // the two octet stars: h carries the non-alternating edge figure, c only alternating ones
    val octet = slab.filter(_.showSupport == "{tet:8 oct:6}")
    octet.count(_.figures.size == 1) shouldBe 1 // c: 12 × alternating
    octet.count(_.figures.size == 2) shouldBe 1 // h: 6 × alternating + 6 × non-alternating
    // the two prism stars: the parallel one carries the all-lateral 6-ring, the mixed one the 5-cell figure
    val prisms = slab.filter(_.showSupport == "{p3:12}")
    prisms.count(_.figures.exists(_._1.size == 6)) shouldBe 1
    prisms.count(_.figures.exists(_._1.size == 5)) shouldBe 1
    // the elongated star: 3 octet-alternating + 6 elongation + 1 all-lateral ring
    val elong  = slab.find(_.showSupport == "{tet:4 oct:3 p3:6}").get
    elong.figures.map((k, m) => (k.size, m)).sorted shouldBe Vector((4, 3), (4, 6), (6, 1))

  "the Krötenheerdt sequence of the slab alphabet" should "be 6, 20, 24, 16, 0" in:
    sequence shouldBe Vector(6, 20, 24, 16, 0)
    classes should have size 66
    classes.map(c => (c.vertical, c.word)).distinct should have size 66

  "the six 1-uniform classes" should "be the six honeycombs of the 28 with slab cells" in:
    val k1     = classes.filter(_.k == 1)
    k1.map(_.showWord) should contain theSameElementsAs Vector("+", "0", "+-", "+0", "+0-0", "g")
    k1.flatMap(_.name) should have size 6
    // the 1-uniform classes of the completeness audit on the slab supports are exactly six as well
    val slab   = Set("{tet:8 oct:6}", "{tet:4 oct:3 p3:6}", "{p3:12}")
    val (a, _) = io.github.scala_tessella.research_core.CompletenessAudit.results
    a.filter(x => slab(io.github.scala_tessella.research_core.SpeciesEnumerator.species(x.idx).showSupport))
      .map(_.classes)
      .sum shouldBe 6

  "the pure-octet sub-world" should "reproduce the Barlow stacking sequence 2, 4, 0" in:
    val octetOnly = verticalClasses.filter(_.word.forall(_ != 0))
    octetOnly.groupMapReduce(_.k)(_ => 1)(_ + _) shouldBe Map(1 -> 2, 2 -> 4)
    octetOnly.filter(_.k == 2).flatMap(_.name) should contain theSameElementsAs Vector(
      "4H stacking (double-hcp, word hc)",
      "6H stacking (word hcc)",
      "9R stacking (samarium-type, word hhc)",
      "12R stacking (word hhcc)"
    )

  "the horizontal world" should "reproduce the Barlow letter combinatorics on {s, g}" in:
    horizontalClassesAll.groupMapReduce(_.k)(_ => 1)(_ + _) shouldBe Map(1 -> 2, 2 -> 4)
    horizontalClassesAll.filter(_.k == 2).map(_.showWord) should contain theSameElementsAs
      Vector("gs", "ggs", "gss", "ggss")
    horizontalClassesAll.foreach(c => c.orbitData.speciesSet.subsetOf(Set(Ppar, Pmix)) shouldBe true)

  "hand-verified anchors" should "match the machinery" in:
    // [+0-]: octet, prism, mirrored octet — junctions h,e,e; 2 orbits
    val hee  = orbits(Vector(1, 0, -1), vertical = true)
    junctions(Vector(1, 0, -1)) shouldBe Vector(H, E, E)
    hee.census shouldBe Vector((H, 1), (E, 2))
    // [++00--]: the k = 4 witness — junctions h,c,e,p,e,c with orbits {h},{c,c},{e,e},{p}
    val w4   = Vector(1, 1, 0, 0, -1, -1)
    junctions(w4) shouldBe Vector(H, C, E, Ppar, E, C)
    orbits(w4, vertical = true).census shouldBe Vector((C, 2), (H, 1), (E, 2), (Ppar, 1))
    // [gs]: the alternating horizontal word is 2-uniform with distinct species x, p
    orbits(Vector(1, 0), vertical = false).census shouldBe Vector((Ppar, 1), (Pmix, 1))
    // negative control: a c-run of length 4 has two c-orbits — 3 orbits, NOT Krötenheerdt
    val run4 = orbits(Vector(1, 1, 1, 1, 0), vertical = true)
    run4.count shouldBe 3
    run4.isKroetenheerdt shouldBe false

  "every class" should "respect the quasi-period bound m ≤ 2k" in:
    classes.foreach(c => minimalQuasiPeriod(c.word) should be <= 2 * c.k)

  "an over-sweep beyond the bound" should "find nothing new" in:
    val knownV = verticalClasses.map(_.word).toSet
    enumerate(verticalWorld = true, 11 to 11).foreach(c => knownV should contain(c.word))
    val knownH = horizontalClassesAll.map(_.word).toSet
    enumerate(verticalWorld = false, 11 to 12).foreach(c => knownH should contain(c.word))
