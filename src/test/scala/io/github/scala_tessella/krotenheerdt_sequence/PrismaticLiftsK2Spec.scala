package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.DelaneySymbols

import io.github.scala_tessella.research_core.SpeciesCorona

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.SymbolCatalog.{canonicalKey, k2Entries}

/** k = 2, LIFT SIDE: THE PRISMATIC-LIFT IDENTIFICATION, NAME BY NAME — the planar 2-uniform Krötenheerdt
  * atlas (A068600 n = 2, re-enumerated by the 2D SAT assembler) lifts tiling by tiling into the k = 2 census.
  * The identification machinery lives in `PrismaticLiftId` (shared with the atlas probe); this spec asserts
  * the theorem: each 2D tiling identifies a UNIQUE species pair, the 20 derived keys are DISTINCT, and every
  * one sits in its pair's census keys. Together with the Barlow and slab identifications (SymbolK2GateSpec)
  * this establishes: the 57 census classes = 20 prismatic lifts + 4 Barlow + 16 slab + EXACTLY 17 NEW k = 2
  * honeycombs.
  *
  * The unguarded canary runs one lift end to end; the full 20-tiling check is guarded (-Dlifts.k2).
  */
class PrismaticLiftsK2Spec extends AnyFlatSpec with Matchers:

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit =
    println(s"[lift ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()

  private val censusKeyCache                                 = collection.mutable.Map.empty[(Int, Int), Set[Vector[Int]]]
  private def censusKeys(iA: Int, iB: Int): Set[Vector[Int]] =
    censusKeyCache.getOrElseUpdate(
      (iA, iB), {
        val (entries, stats) = k2Entries(iA, iB, maxChambers = 128, sigma0Cap = 100000)
        stats.exists(_.capped) shouldBe false
        entries.map(e => canonicalKey(e.sym)).toSet
      }
    )

  /** Identify one tiling: assert the identification is unique and return (spA, spB, key). */
  private def uniqueLift(name: String, ds: DelaneySymbols.DSymbol): (Int, Int, Vector[Int]) =
    val (types, hits) = PrismaticLiftId.identify(ds)
    withClue(s"$name: exactly two vertex types: ")(types should have size 2)
    withClue(s"$name: unique species identification, got ${hits.map(h => (h._1, h._2))}: "):
      hits should have size 1
    hits.head

  "one prismatic lift" should "identify and land in the census (canary: the first 2D 2-uniform tiling)" in:
    val (name, sym)   = PrismaticLiftId.the20().head
    say(s"canary lift: $name")
    val (iA, iB, key) = uniqueLift(name, sym)
    say(s"  identified pair ${SpeciesCorona.label(iA)} ~ ${SpeciesCorona.label(iB)}")
    censusKeys(iA, iB) should contain(key)

  "THE PRISMATIC-LIFT GATE" should
    "land all 20 planar 2-uniform lifts in the census, name by name, distinct (enable with -Dlifts.k2)" in:
      assume(sys.props.contains("lifts.k2"), "20 lifts + per-pair censuses — enable with -Dlifts.k2")
      val tilings = PrismaticLiftId.the20()
      tilings should have size 20
      val keys    = tilings.map { (name, sym) =>
        say(s"lift $name")
        val (iA, iB, key) = uniqueLift(name, sym)
        val lbl           = s"${SpeciesCorona.label(iA)} ~ ${SpeciesCorona.label(iB)}"
        say(s"  -> $lbl")
        withClue(s"$name -> $lbl: ")(censusKeys(iA, iB) should contain(key))
        (name, (iA, iB), key)
      }
      keys.map(_._3).distinct should have size 20
      val byPair  = keys.groupBy(_._2).view.mapValues(_.size).toMap
      say(s"lifts per pair: ${byPair.map((p, n) => s"${p._1}:${p._2}=$n").toVector.sorted.mkString(" ")}")
      // 15 distinct species pairs carry the 20 lifts — counted from the 2D atlas's distinct type pairs;
      // a count of 14 would miss one: the pairs are counted here from the tilings themselves
      byPair.keySet should have size 15
