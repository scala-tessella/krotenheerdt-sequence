package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StripStacking.*
import StripStacking.Layer.*
import StripSymmetry.*
import StripEnumeration.*

/** The enumeration primitives on hand examples, and the whole search at k = 1. */
class StripEnumerationSpec extends AnyFlatSpec with Matchers:

  "the letters" should "be the cubic layer, two plain rows and a hexagon row per axis, period and offset" in {
    letters(2).size shouldBe 7
    letters(3).size shouldBe 9
    letters(Set(2, 3)).size shouldBe 13
    letters(Set(2, 3, 4, 6)).size shouldBe 33
  }

  "the axis periods" should "be the lcm of the hexagon periods per axis" in {
    axisPeriods(parseWord("C Tu{0/2} Tu Tw{0/3} Tw Tw{0/4} Tw")) shouldBe (2, 12)
    axisPeriods(parseWord("C Tu Tw")) shouldBe (1, 1)
    effectiveGroup(parseWord("C Tu{0/2} Tu Tw")).size shouldBe 8 * 4 * 2 // lcm 2 on u, none on w
  }

  "a plain level" should
    "be one no hexagon half touches, and forbid axis periods outside 1, 2, 4 in its word" in {
      plainLevel(parseWord("C C Tu"), 1, closed = true) shouldBe Some(true)                 // cube on cube
      plainLevel(parseWord("C Tu Tw"), 2, closed = true) shouldBe Some(true)                // plain row on plain row
      plainLevel(parseWord("Tu Tw{0/3} Tw"), 1, closed = true) shouldBe Some(false)         // hexagons above
      plainLevel(parseWord("Tw{0/3} Tw Tu"), 2, closed = true) shouldBe Some(false)         // upper halves below
      plainLevel(parseWord("Tw Tu"), 1, closed = false) shouldBe None                       // the row below is unknown
      plainLevelsOk(parseWord("Tu Tw{0/3} Tw{1/3} Tw{2/3} Tw"), closed = true) shouldBe true
      plainLevelsOk(parseWord("C Tu{0/4} Tu Tw Tu{1/4} Tu"), closed = true) shouldBe true
      plainLevelsOk(parseWord("C C Tu{0/3} Tu{1/3} Tu"), closed = true) shouldBe
        false                                                                               // cube on cube with period 3
      plainLevelsOk(parseWord("Tu Tu Tw{0/4} Tw"), closed = true) shouldBe true             // row on row with period 4
      plainLevelsOk(parseWord("C Tw{1/4} Tw Tu Tw{0/4} Tw C"), closed = true) shouldBe true // the glide word
      plainLevelsOk(parseWord("C C Tw{0/3} Tw Tu"), closed = false) shouldBe false          // prefix: period 3 stays
      plainLevelsOk(parseWord("C C Tw{0/2} Tw Tu{0/3} Tu"), closed = true) shouldBe
        false                                                                               // period 3 on one axis
      plainLevelsOk(parseWord("C C C Tu{0/2} Tu Tw{0/2} Tw"), closed = true) shouldBe
        true                                                                                // period 2: vacuous
    }

  "a base row" should "be a hexagon row whose lower level carries no same-axis hexagon half" in {
    baseRow(parseWord("C Tu{0/2} Tu"), 1, closed = true) shouldBe Some(true)             // over a cube layer
    baseRow(parseWord("Tw Tu{0/3} Tu"), 1, closed = true) shouldBe Some(true)            // over the other axis
    baseRow(parseWord("Tu{0/3} Tu Tu{0/3} Tu"), 2, closed = true) shouldBe Some(false)   // upper halves below
    baseRow(parseWord("Tu{0/3} Tu{1/3} Tu{2/3}"), 1, closed = true) shouldBe Some(false) // a mid-level
    baseRow(parseWord("Tu Tu{0/2}"), 1, closed = false) shouldBe None                    // two below unknown
    baseRow(parseWord("C Tu"), 1, closed = true) shouldBe None                           // no hexagons
  }

  "the single-period lemma" should "hold the periods of one axis to divisors of its base period" in {
    // the mixed k = 5 class: period 2 along u, period 4 along w
    axisPeriodsOk(parseWord("Tu{0/2} Tu Tw{0/4} Tw Tu{1/2} Tu Tw{1/4} Tw"), closed = true) shouldBe true
    axisPeriodsOk(parseWord("Tu Tw{0/3} Tw{1/3} Tw{2/3} Tw"), closed = true) shouldBe true // the 6³ block
    axisPeriodsOk(parseWord("C Tu{0/4} Tu Tu{0/2} Tu Tw"), closed = true) shouldBe true    // 2 divides 4
    axisPeriodsOk(parseWord("C Tu{0/2} Tu Tw Tu{0/3} Tu"), closed = true) shouldBe false   // base periods 2, 3
    axisPeriodsOk(parseWord("C Tu{0/2} Tu Tw Tu{0/4} Tu"), closed = true) shouldBe false   // base periods 2, 4
    axisPeriodsOk(parseWord("Tu Tw{0/6} Tw"), closed = true) shouldBe false                // a base row of period 6
    axisPeriodsOk(parseWord("Tu{0/3} Tu{1/3} Tu{2/3}"), closed = true) shouldBe true       // one direction: a lift
    axisPeriodsOk(parseWord("Tw Tu{0/2} Tu Tu{0/3}"), closed = false) shouldBe false       // open: 2 and 3 on u
    axisPeriodsOk(parseWord("Tw Tu{0/2} Tu Tu{0/4}"), closed = false) shouldBe
      false                                                                                // open: base period 2, then 4
    axisPeriodsOk(parseWord("Tu Tu{0/2} Tu Tu{0/4}"), closed = false) shouldBe
      true                                                                                 // open, no readable base: {2, 4}
  }

  "the vertex orbits of a word" should "be its k orbits, one species each, every vertex assigned" in {
    // the mixed 7-uniform word: seven orbits, seven distinct species
    val (byVertex, species) = VertexOrbits.of(parseWord("C Tw{0/4} Tw Tu{1/2} Tu Tw{1/4} Tw")).get
    species.size shouldBe 7
    species.flatten.toSet.size shouldBe 7
    byVertex.values.toSet shouldBe (0 until 7).toSet
    VertexOrbits.krotenheerdt(parseWord("C Tw{0/4} Tw Tu{1/2} Tu Tw{1/4} Tw")) shouldBe Some(7)
    VertexOrbits.krotenheerdt(parseWord("C Tw{0/4} Tw Tu Tw{0/4} Tw C")) shouldBe
      None // 14 orbits, species repeated
  }

  "the seven-species filter" should "cut period-2 words and two period-2 base axes for k >= 8 only" in {
    highKOk(parseWord("C C Tu{0/2} Tu Tw"), 7, closed = false) shouldBe true     // vacuous below 8
    highKOk(parseWord("C C Tu{0/2} Tu Tw"), 8, closed = false) shouldBe
      true                                                                       // a cube on a cube: w may reach period 4
    highKOk(parseWord("C C Tu{0/2} Tu Tw"), 8, closed = true) shouldBe false     // closed with periods 2 and 1
    highKOk(parseWord("C Tw{1/4} Tw Tu Tw{0/4} Tw C"), 8, closed = true) shouldBe
      true                                                                       // plain level, period 4
    highKOk(parseWord("Tw{0/3} Tw Tu Tw{0/3} Tw"), 8, closed = true) shouldBe
      true                                                                       // the carrier rows carry halves: no plain level
    highKOk(parseWord("Tw{0/3} Tw Tu Tu Tw{0/3} Tw"), 8, closed = true) shouldBe
      true                                                                       // Tu on Tu with period 3: plainLevelsOk's cut
    highKOk(parseWord("Tw{0/3} Tw Tu{0/2} Tu Tw{1/3} Tw"), 8, closed = true) shouldBe
      true                                                                       // no plain level, periods 3 and 2
    highKOk(parseWord("Tw{0/2} Tw Tu{0/2} Tu"), 8, closed = true) shouldBe false // base period 2 on both axes
    highKOk(parseWord("C Tw{0/4} Tw Tu{1/2} Tu Tw{1/4} Tw"), 8, closed = true) shouldBe
      true                                                                       // the mixed k = 7 shape
  }

  "the vertices per period of a level" should
    "count the lcm of the meeting pairs less the mid-level hexagons" in {
      val hex = parseWord("Tu{0/3} Tu{1/3} Tu{2/3}") // the hexagonal prismatic honeycomb: every level 3 − 1
      levelVertices(hex, 0, closed = true) shouldBe Some(2)
      val kag = parseWord("Tu Tu{0/2}")              // kagome lift: the mid-level 2 − 1, the pair boundary 2
      levelVertices(kag, 0, closed = true) shouldBe Some(1)
      levelVertices(kag, 1, closed = true) shouldBe Some(2)
      levelVertices(parseWord("C Tu Tw"), 2, closed = true) shouldBe None // rows of different axes
      levelVertices(parseWord("Tu{0/3} Tu{1/3} Tu"), 2, closed = false) shouldBe Some(2)
      levelVertices(parseWord("Tu{0/3} Tu{1/3} Tu"), 1, closed = false) shouldBe
        None // the row below is unknown
      levelsOk(hex, 1, closed = true) shouldBe true
      levelsOk(parseWord("Tu Tu{0/4} Tu Tu"), 1, closed = true) shouldBe
        false // a sparse pair: 4 vertices on a level
      levelsOk(parseWord("Tu Tu{0/4} Tu Tu"), 2, closed = true) shouldBe true
    }

  "the prefix filter" should "accept a hexagon row after a hexagon row and reject one before a cube" in {
    prefixConsistent(parseWord("Tu Tw{0/3} Tw{1/3}")) shouldBe true
    prefixConsistent(parseWord("Tu{0/2} C")) shouldBe false
    prefixConsistent(parseWord("Tu{0/2} Tw")) shouldBe false
  }

  "the oracle" should "read the junctions of the 108-world word" in {
    val o = SpeciesOracle()
    val w = parseWord("Tw{0/2} Tw Tu{0/2} Tu C C")
    o.junction(w, 0) shouldBe Set(16)
    o.junction(w, 1) shouldBe Set(21)
    o.junction(w, 2) shouldBe Set(20)
    o.junction(w, 5) shouldBe Set(18)
    o.interior(parseWord("C C Tu{0/2} Tu Tw")) shouldBe Vector(Set(16), Set(21))
  }

  "the open prefix filter" should "count the species seen so far" in {
    val o = SpeciesOracle()
    prefixOk(parseWord("C C Tu{0/2} Tu Tw"), 2, o) shouldBe true
    prefixOk(parseWord("C C Tu{0/2} Tu Tw"), 1, o) shouldBe false
  }

  it should "reject a species at three junctions and two shared-species pairs with different centres" in {
    val o = SpeciesOracle()
    prefixOrbitsOk(parseWord("C C C C C C"), 6, o) shouldBe false       // {cube:8}#1 at junctions 2, 3, 4
    prefixOrbitsOk(parseWord("C C C C C"), 6, o) shouldBe true          // at 2 and 3 only: a mirror pair
    prefixOrbitsOk(parseWord("Tu Tw Tu Tw Tu Tw"), 6, o) shouldBe false // {p3:12}#1 three times
    // {cube:8}#1 at 2 and 6, {cube:4 p3:6}#1 at 3 and 5 (both centred on the Tu|Tu junction 4): compatible
    prefixOrbitsOk(parseWord("C C C Tu Tu C C C"), 6, o) shouldBe true
    // one more cube layer: {cube:8}#1 at 2, 6 and 7 — three positions
    prefixOrbitsOk(parseWord("C C C Tu Tu C C C C"), 6, o) shouldBe false
  }

  "the species union" should "read every junction and stop at k" in {
    val o = SpeciesOracle()
    speciesUnion(parseWord("Tw{0/2} Tw Tu{0/2} Tu C C"), 4, o) shouldBe Some(Set(16, 18, 20, 21))
    speciesUnion(parseWord("Tw{0/2} Tw Tu{0/2} Tu C C"), 3, o) shouldBe None
  }

  "the closed filter" should "pass Krötenheerdt words at their k and reject repeated species" in {
    val o = SpeciesOracle()
    val g = inPlaneGroup(2)
    closedOk(parseWord("Tw{0/2} Tw Tu{0/2} Tu C C"), 4, o, g).map(_.map(_.size).sum) shouldBe Some(4)
    closedOk(parseWord("Tw{0/2} Tw Tu{0/2} Tu C C"), 3, o, g) shouldBe None
    closedOk(parseWord("C C Tu C C C Tw"), 4, o, g) shouldBe None // two orbits of {cube:8}#1
  }

  /** The counterexample to the first plain-level lemma: a cube on a cube and period-4 rows, 7-uniform, its
    * plain level one orbit under a glide across it by two units and a reflection about a half-integer.
    */
  val glideWord: Vector[Layer] = parseWord("C Tw{1/4} Tw Tu Tw{0/4} Tw C")

  "the glide word" should "be a 7-uniform Krötenheerdt stacking with a plain level and period 4" in {
    plainLevel(glideWord, 0, closed = true) shouldBe Some(true)
    axisPeriods(glideWord) shouldBe (1, 4) // the u-rows plain, the w-rows of period 4
    val (_, species) = VertexOrbits.of(glideWord).get
    species.size shouldBe 7
    species.forall(_.size == 1) shouldBe true
    species.flatten.toSet shouldBe Set(16, 18, 25, 27, 28, 29, 30)
    closedOk(glideWord, 7, SpeciesOracle(), effectiveGroup(glideWord)).map(_.map(_.size).sum) shouldBe Some(7)
  }

  "the block condition" should "require junction sets that meet to be equal" in {
    blocksOk(Vector(Set(19, 28), Set(19, 28), Set(21), Set(16, 25))) shouldBe true
    blocksOk(Vector(Set(19, 28), Set(28, 30))) shouldBe false // 28 shared, sets differ
    blocksOk(Vector.empty[Set[Int]]) shouldBe true
    val o      = SpeciesOracle()
    blocksOk(glideWord.indices.map(j => o.junction(glideWord, j))) shouldBe true
    val kagome = parseWord("Tw{0/2} Tw Tu{0/2} Tu C C") // the 4-uniform kagome word
    blocksOk(kagome.indices.map(j => o.junction(kagome, j))) shouldBe true
  }

  "the period-4 runs cut" should
    "forbid a third consecutive period-4 hexagon row in two-direction words only" in {
      runs4Ok(parseWord("Tu Tw{0/4} Tw{1/4} Tw{3/4} Tw"), closed = false) shouldBe false // three in a row
      runs4Ok(parseWord("Tu Tw{0/4} Tw{1/4} Tw"), closed = false) shouldBe true          // two are allowed
      runs4Ok(parseWord("Tu{0/4} Tu{1/4} Tu{3/4} Tu"), closed = true) shouldBe true      // a lift: vacuous
      runs4Ok(parseWord("Tw{1/4} Tw{3/4} Tw Tu Tw{0/4}"), closed = true) shouldBe false  // across the wrap
      runs4Ok(parseWord("Tu Tw{1/3} Tw{2/3} Tw{0/3} Tw"), closed = true) shouldBe
        true                                                                             // period 3: the 6³ block
      runs4Ok(glideWord, closed = true) shouldBe true
    }

  "the glide cut" should "keep a plain level fixed by a stack reversal and cut one that is not" in {
    needsGlide(glideWord) shouldBe true
    plainLevels(glideWord, closed = true) shouldBe Vector(0) // the cube on the cube
    glideLevelsOk(glideWord, effectiveGroup(glideWord)) shouldBe true
    val noFlip    = parseWord("C C Tw{0/4} Tw Tu") // nothing mirrors C|C
    plainLevels(noFlip, closed = true) should contain(1)
    glideLevelsOk(noFlip, effectiveGroup(noFlip)) shouldBe false
    closedOk(noFlip, 7, SpeciesOracle(), effectiveGroup(noFlip)) shouldBe None
    val periodTwo = parseWord("C C Tu{0/2} Tu Tw") // lcms 2 and 1: vacuous
    needsGlide(periodTwo) shouldBe false
    glideLevelsOk(periodTwo, effectiveGroup(periodTwo)) shouldBe true
    glideLevelsOk(parseWord("C Tw{0/4} Tw"), effectiveGroup(parseWord("C Tw{0/4} Tw"))) shouldBe
      true // a lift
  }

  it should "compare the species at junctions mirrored about a readable plain level of a prefix" in {
    val o      = SpeciesOracle()
    val mirror = parseWord("Tu Tw{0/4} Tw C C Tw{1/4} Tw Tu") // the glide word about C|C
    plainLevels(mirror, closed = false) shouldBe Vector(4)
    prefixGlideOk(mirror, o.interior(mirror)) shouldBe true
    val broken = parseWord("Tu Tw{0/4} Tw C C Tw Tu Tu")      // Tw|C against C|Tw
    prefixGlideOk(broken, o.interior(broken)) shouldBe false
    val short  = parseWord("Tu Tw{0/4} Tw C C")               // no mirrored pair readable
    prefixGlideOk(short, o.interior(short)) shouldBe true
    prefixOk(mirror, 7, o) shouldBe true
    prefixOk(broken, 7, o) shouldBe false
  }

  it should "be found by the search at k = 7 over period 4" in {
    val target = canonicalKey(symbolOfLayers(glideWord).get)
    val found  = java.util.concurrent.ConcurrentHashMap.newKeySet[Vector[Int]]()
    enumerate(
      7,
      Set(4),
      7,
      7,
      SpeciesOracle(),
      (w, _) => symbolOfLayers(w).foreach(x => found.add(canonicalKey(x))),
      threads = 4,
      rootLen = 3,
      progress = None
    )
    found.contains(target) shouldBe true
  }

  "the search at k = 1" should "produce exactly the six 1-uniform stackings of hexagon period 2" in {
    val o    = SpeciesOracle()
    val keys = collection.mutable.Map.empty[Vector[Int], String]
    enumerate(
      1,
      2,
      2,
      8,
      o,
      (w, _) =>
        symbolOfLayers(w).foreach { x =>
          if x.orbitsOf(Vector(1, 2, 3)).max == 0 then keys.getOrElseUpdate(canonicalKey(x), showLayers(w))
        }
    )
    // the cubic, elongated and gyroelongated triangular prismatic, triangular prismatic, kagome, gyrated
    keys.values.toVector.sorted shouldBe Vector("C", "C Tu", "C Tu C Tw", "Tu", "Tu Tu{0/2}", "Tu Tw")
  }

  it should "give the same six keys on four worker threads (the representative words may differ)" in {
    def run(threads: Int): Set[Vector[Int]] =
      val o    = SpeciesOracle()
      val keys = collection.mutable.Set.empty[Vector[Int]]
      enumerate(
        1,
        2,
        2,
        8,
        o,
        (w, _) =>
          symbolOfLayers(w).foreach { x =>
            if x.orbitsOf(Vector(1, 2, 3)).max == 0 then keys.synchronized(keys += canonicalKey(x))
          },
        threads = threads
      )
      keys.toSet
    val par                                 = run(4)
    par.size shouldBe 6
    par shouldBe run(1)
  }
