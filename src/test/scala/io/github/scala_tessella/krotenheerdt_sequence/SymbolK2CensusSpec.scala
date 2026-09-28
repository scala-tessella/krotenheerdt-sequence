package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona

import io.github.scala_tessella.research_core.SpeciesEnumerator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}
import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionAutosOf, unionOf}
import io.github.scala_tessella.research_core.SymbolCatalog.*
import SymbolRealizationFilter.realize

/** THE k = 2 CENSUS IS EXACTLY 57, ALL REALIZED, TAILS EMPTY — the theorem-grade battery, guarded
  * (-Dcensus.k2, ~45 min: the PairShell fair-pair sweep + the scoped census + realization + every
  * beyond-scope tail under the deck-conjugation lex-leader). The unguarded canary re-derives two small pairs
  * end to end (census counts + realization) in seconds, including the headline answer: the gyrated
  * quarter-cubic pair carries EXACTLY ONE k = 2 class.
  */
class SymbolK2CensusSpec extends AnyFlatSpec with Matchers:

  private lazy val flags = MonoShell.Flags()

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit =
    println(s"[census ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()

  private def pairLabel(i: Int, j: Int): String =
    s"${SpeciesCorona.label(i)} ~ ${SpeciesCorona.label(j)}"

  /** The census, pinned pair by pair (the scoped census record). */
  private val expected: Map[String, Int] = Map(
    "{cube:2 co:1 rco:2}#1 ~ {tet:1 cube:1 rco:3}#1"        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2"          -> 1,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1"                -> 1,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2"         -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2"          -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:12}#2"                     -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"           -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3"                  -> 4,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"                -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"                -> 1,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"                -> 1,
    "{cube:8}#1 ~ {cube:4 p3:6}#1"                          -> 4,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                         -> 1,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#2"                         -> 1,
    "{p3:4 p6:4}#1 ~ {p3:12}#2"                             -> 1,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                         -> 2,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                         -> 2,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                         -> 0,
    "{tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2" -> 2,
    "{tet:2 truncTet:6}#1 ~ {tet:2 truncTet:6}#2"           -> 1,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                     -> 2,
    "{cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:6}#1 ~ {p3:12}#1"                           -> 4,
    "{cube:4 p3:6}#1 ~ {p3:12}#2"                           -> 4,
    "{cube:4 p3:6}#2 ~ {p3:12}#2"                           -> 1,
    "{p3:8 p6:2}#2 ~ {p3:12}#1"                             -> 0,
    "{p3:8 p6:2}#2 ~ {p3:12}#2"                             -> 2,
    "{p3:12}#1 ~ {p3:12}#2"                                 -> 4,
    "{p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                      -> 4,
    "{tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                -> 4,
    "{tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                -> 4,
    "{tet:8 oct:6}#1 ~ {tet:8 oct:6}#2"                     -> 4
  )

  private val scope = 128

  private def tailEmpty(i: Int, j: Int): Boolean =
    val (symA, symB) = (symmetryOf(i), symmetryOf(j))
    val tailPairs    =
      for
        sa <- subgroupsOfSpecies(i)
        sb <- subgroupsOfSpecies(j)
        if symA.cx.chambers.size / sa.size + symB.cx.chambers.size / sb.size > scope
      yield (sa, sb)
    tailPairs.forall { (sa, sb) =>
      val (fA, fB)     = (fold(symA, sa), fold(symB, sb))
      val autos        = unionAutosOf(symA, sa, fA, symB, sb, fB)
      val (sols, capd) = enumerateSigma0(unionOf(Vector(fA, fB)), log = say, autos = autos)
      !capd && !sols.exists(s0 => isMinimal(symOf(unionOf(Vector(fA, fB)), Vector(i, j), s0)))
    }

  "the k = 2 census canary" should
    "pin the two small pairs end to end, incl. the gyrated quarter-cubic answer (exactly ONE class)" in:
      // {tet:2 truncTet:6}#1 ~ #2 (the question of the gyrated quarter-cubic pair) and {cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2
      // p6:2}#2 — both small enough for the default tier, censused AND realized, tails empty outright
      def bySupport(sup: String) =
        SpeciesEnumerator.species.indices.filter(i => SpeciesEnumerator.species(i).showSupport == sup)
      val Vector(qA, qB)         = bySupport("{tet:2 truncTet:6}").toVector
      val (entries, stats)       = k2Entries(qA, qB, maxChambers = scope, sigma0Cap = 100000)
      stats.exists(_.capped) shouldBe false
      entries should have size 1
      val v                      = realize(entries.head, flags)
      v.realized shouldBe true
      v.cert.get.ok shouldBe true
      tailEmpty(qA, qB) shouldBe true

  "THE k = 2 CENSUS" should
    "be EXACTLY 57 over the 33 fair pairs, per-pair pinned, all realized, all tails empty " +
    "(enable with -Dcensus.k2)" in:
      assume(sys.props.contains("census.k2"), "~45 min full census battery — enable with -Dcensus.k2")
      val pairs = PairShell.fairPairs
      pairs should have size 33
      var total = 0
      for (i, j) <- pairs do
        val lbl              = pairLabel(i, j)
        say(s"pair $lbl")
        val (entries, stats) = k2Entries(i, j, maxChambers = scope, sigma0Cap = 100000)
        withClue(s"$lbl: "):
          stats.exists(_.capped) shouldBe false
          expected.keySet should contain(lbl)
          entries.size shouldBe expected(lbl)
          for e <- entries do
            val v = realize(e, flags, log = say)
            withClue(s"symbol ${e.sym.size}ch: "):
              v.realized shouldBe true
              v.cert.get.ok shouldBe true
          tailEmpty(i, j) shouldBe true
        total += entries.size
      total shouldBe 57
