package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona
import io.github.scala_tessella.research_core.StarFoldings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.SpeciesEnumerator.species
import io.github.scala_tessella.research_core.SymbolCatalog.*
import SymbolRealizationFilter.realizeK

/** the census k = 4 pinning (transposing `SymbolK3CensusSpec`): THE k = 4 CENSUS IS EXACTLY 130 OVER THE 339
  * FAIR QUADRUPLES AT SCOPE 128, ALL REALIZED, AND THE (128, 160] BAND HOLDS EXACTLY 12 MINIMAL SYMBOLS ON
  * FOUR SETS, ALL AT 144 CHAMBERS, ALL 12 REALIZED — the guarded battery (-Dcensus.k4: the KSetShell
  * fair-quadruple sweep + the scoped census + realization + the band under the k-part deck-conjugation
  * lex-leader + the 16 certified slab keys located; expect ~9 h at par=8, dominated by the fairness sweep (~2
  * h 45 m) and the band monsters). The residue (160, 176] is not yet part of this battery. The unguarded
  * canary pins a small quadruple end to end in ~20 s: 9:16:19:25 carries EXACTLY ONE k = 4 class, realized,
  * band empty (27 tuples); the rich 10-class set 16:21:25:27 is pinned by the battery.
  */
class SymbolK4CensusSpec extends AnyFlatSpec with Matchers:

  private lazy val flags = MonoShell.Flags()

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit = synchronized {
    println(s"[k4census ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()
  }

  private def quadLabel(sps: Vector[Int]): String =
    sps.map(SpeciesCorona.label).mkString(" ~ ")

  private val scope  = 128
  private val bandHi = 160

  /** The census, pinned quadruple by quadruple (the scoped census record). */
  private val expected: Map[String, Int] = Map(
    "{cube:2 p6:2 p12:2}#1 ~ {p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2"       -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2"        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2"        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2" -> 1,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1"      -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2"      -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#2"            -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"  -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"       -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"       -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"       -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"       -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:12}#2"             -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:12}#2"                    -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"            -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"              -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                  -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                  -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                    -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"             -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1"             -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3"          -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3"                    -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#2"                  -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                 -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1"               -> 1,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2"               -> 1,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                 -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                 -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                 -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1"                  -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                   -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                   -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2"        -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1"             -> 2,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2"             -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#2"                   -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"         -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"              -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"              -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"              -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                   -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                         -> 1,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"         -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"              -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"              -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"              -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:12}#2"                    -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                   -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                         -> 1,
    "{p6:6}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:12}#2"                                 -> 0,
    "{p6:6}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                              -> 0,
    "{p6:6}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1"                            -> 2,
    "{p6:6}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2"                            -> 1,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                                     -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                                     -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                                     -> 2,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:12}#2"                                         -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                                   -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                                       -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                                       -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                         -> 2,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                         -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                         -> 4,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                             -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                                  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"       -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"       -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"       -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:12}#2"             -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1"                 -> 1,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:12}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"            -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"              -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                        -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"             -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1"             -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3"          -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"        -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"        -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#2"                  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1"                  -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                   -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                       -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1"                        -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                        -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                              -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"        -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                 -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                     -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1"                  -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"             -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                       -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                         -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                             -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:12}#2"                            -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                      -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                            -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                     -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                      -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                            -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                            -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1"                        -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                        -> 1,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                          -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1 ~ {p3:12}#2"                              -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                              -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                     -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                         -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                   -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                   -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {p3:4 p6:4}#3"                     -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1"                   -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#2"                   -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                  -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1"                -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2"                -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                  -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1"                -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"                -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                  -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                  -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"              -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                    -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                    -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                    -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                            -> 6,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1"                          -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2"                          -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                            -> 6,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1"                          -> 2,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"                          -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                            -> 8,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                            -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                        -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                          -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                          -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                              -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1"                       -> 2,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:12}#2"                             -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                     -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2" -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                     -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1"                       -> 10,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                           -> 5,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#1"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                             -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                             -> 8,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                             -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                             -> 2,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                     -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                           -> 2,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                               -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                    -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                        -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                          -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                              -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                              -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1"                       -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                     -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1"                         -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                         -> 2,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                           -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1 ~ {p3:12}#2"                               -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                    -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                           -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                               -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                    -> 0,
    "{cube:8}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                                 -> 0,
    "{cube:8}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                                     -> 0,
    "{cube:8}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1"                                 -> 4,
    "{cube:8}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                                 -> 0,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                               -> 0,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1"                                   -> 0,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                                   -> 1,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                     -> 0,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                     -> 0,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                         -> 16,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                              -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                                -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                                -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:12}#2"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2"        -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1"                              -> 2,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                              -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2"                                -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                                    -> 4,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                    -> 1,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                             -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                            -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                                -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                      -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                           -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                      -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                           -> 0,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 0,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 0,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                             -> 0,
    "{p3:4 p6:4}#1 ~ {p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                                 -> 0,
    "{p3:4 p6:4}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                           -> 0,
    "{p3:4 p6:4}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                           -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2"        -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1"                              -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                              -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2"                                -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                                    -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                    -> 0,
    "{p3:4 p6:4}#2 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#1 ~ {tet:2 truncTet:6}#2" -> 4,
    "{p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2 ~ {p3:8 p6:2}#1"        -> 0,
    "{p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2 ~ {p3:8 p6:2}#2"        -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#1"                            -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                            -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2"                              -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                  -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                                  -> 2,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                  -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                    -> 2,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 1,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                             -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                             -> 0,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                -> 0,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                -> 0,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                    -> 0,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                         -> 0,
    "{cube:4 p3:6}#1 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                  -> 0,
    "{cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                      -> 0,
    "{cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                           -> 0,
    "{cube:4 p3:6}#1 ~ {p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                               -> 0,
    "{cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                         -> 0,
    "{cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                         -> 0,
    "{cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                      -> 0,
    "{cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                           -> 0,
    "{cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                               -> 0,
    "{cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                         -> 0,
    "{cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                         -> 0,
    "{p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 2,
    "{p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                                 -> 0,
    "{p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                           -> 0,
    "{p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                           -> 0,
    "{p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                               -> 0,
    "{p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                               -> 0,
    "{p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1 ~ {tet:8 oct:6}#2"                         -> 16
  )

  /** The (128, 160] band, pinned per set: 12 minimal symbols on four sets, every one at 144 = 36*4. */
  private val expectedBand: Map[String, (Int, Int)] = Map( // label -> (band symbols, realized)
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3" -> (2, 2),
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3" -> (6, 6),
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"         -> (2, 2),
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"         -> (2, 2)
  )

  /** Census + realization + band of one fair quadruple, everything the row claims, as data. */
  final private case class Outcome(
      sps: Vector[Int],
      capped: Boolean,
      keys: Set[Vector[Int]],
      count: Int,
      unrealized: Int,
      certBad: Int,
      bandSymbols: Int,
      bandRealized: Int,
      bandSizes: Set[Int],
      bandCapped: Boolean
  )

  private def outcomeOf(sps: Vector[Int], log: String => Unit): Outcome =
    val (entries, stats) = kEntries(sps, maxChambers = scope, sigma0Cap = 100000)
    val verdicts         = entries.map(e => realizeK(e, flags, log = log))
    val band             = CensusBands.runKSet(sps, scope, bandHi, log)
    Outcome(
      sps,
      stats.exists(_.capped),
      entries.map(e => canonicalKey(e.sym)).toSet,
      entries.size,
      verdicts.count(!_.realized),
      verdicts.count(v => v.realized && !v.cert.exists(_.ok)),
      band.entries.size,
      band.entries.count(_._2.realized),
      band.entries.map(_._1.sym.size).toSet,
      band.sweepCapped
    )

  "the k = 4 census canary" should
    "pin 9:16:19:25 end to end: EXACTLY ONE class, realized, band empty" in:
      def bySupport(sup: String) =
        species.indices.toVector.filter(i => species(i).showSupport == sup)
      val sps                    = Vector(
        bySupport("{cube:2 p6:2 p12:2}").min, // #1
        bySupport("{cube:4 p3:2 p6:2}").min,  // #1
        bySupport("{p3:4 p6:4}").min,         // #1
        bySupport("{cube:4 p3:6}").min        // #1
      ).sorted
      quadLabel(sps) shouldBe "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1"
      val o                      = outcomeOf(sps, say)
      o.capped shouldBe false
      o.count shouldBe 1
      o.unrealized shouldBe 0
      o.certBad shouldBe 0
      o.bandSymbols shouldBe 0
      o.bandCapped shouldBe false

  "THE k = 4 CENSUS + BAND" should
    "be EXACTLY 130 + 12 over the 339 fair quadruples, per-set pinned, band all at 144, " +
    "the 16 slab keys located (enable with -Dcensus.k4)" in:
      assume(sys.props.contains("census.k4"), "multi-hour full k = 4 battery — enable with -Dcensus.k4")
      say("deriving the fair quadruples (the KSetShell sweep, ~2 h 45 m)...")
      val quads = KSetShell.fairKSets(4, flags).filter(_.fair).map(_.kSet)
      quads should have size 339
      quads.map(quadLabel).toSet shouldBe expected.keySet

      // the per-set work runs on a fixed pool (-Dcensus.par, default 8); all assertions on the main thread
      val par  = sys.props.get("census.par").map(_.toInt).getOrElse(8).max(1)
      for i <- quads.flatten.distinct.sorted do
        StarFoldings.subgroupsOfSpecies(i) // pre-warm shared caches sequentially
      val out  = new java.util.concurrent.ConcurrentHashMap[Vector[Int], Outcome]
      val errs = new java.util.concurrent.ConcurrentHashMap[Vector[Int], String]
      val pool = java.util.concurrent.Executors.newFixedThreadPool(par)
      val gate = new java.util.concurrent.CountDownLatch(quads.size)
      for sps <- quads do
        pool.submit(new Runnable:
          def run(): Unit =
            try
              val o   = outcomeOf(sps, _ => ())
              val lbl = quadLabel(sps)
              out.put(sps, o)
              // the whole outcome in the log, so that an interrupted battery still records every finished set
              say(
                s"done $lbl (${out.size}/${quads.size}): classes ${o.count} (pinned ${expected(lbl)}), " +
                  s"unrealized ${o.unrealized}, bad certificates ${o.certBad}, band ${o.bandSymbols} symbols " +
                  s"${o.bandRealized} realized at ${o.bandSizes.toVector.sorted.mkString(",")}, " +
                  s"capped ${o.capped || o.bandCapped}"
              )
            catch case e: Throwable => errs.put(sps, e.toString)
            finally gate.countDown())
      gate.await()
      pool.shutdown()
      errs shouldBe empty

      // every set is checked before anything fails, so one stale pin cannot hide the rest of the battery
      var total                                                      = 0
      var bandTotal                                                  = 0
      val problems                                                   = collection.mutable.ArrayBuffer.empty[String]
      def check[A](lbl: String, what: String, got: A, want: A): Unit =
        if got != want then problems += s"$lbl: $what $got, expected $want"
      for sps <- quads do
        val lbl            = quadLabel(sps)
        val o              = out.get(sps)
        val (bSyms, bReal) = expectedBand.getOrElse(lbl, (0, 0))
        check(lbl, "capped", o.capped, false)
        check(lbl, "classes", o.count, expected(lbl))
        check(lbl, "unrealized", o.unrealized, 0)
        check(lbl, "bad certificates", o.certBad, 0)
        check(lbl, "band symbols", o.bandSymbols, bSyms)
        check(lbl, "band realized", o.bandRealized, bReal)
        if bSyms > 0 then check(lbl, "band sizes", o.bandSizes, Set(144)) // every band discovery at 36k
        check(lbl, "band capped", o.bandCapped, false)
        total += o.count
        bandTotal += o.bandSymbols
      problems.foreach(say)
      withClue(problems.mkString("\n"))(problems shouldBe empty)
      total shouldBe 130
      bandTotal shouldBe 12

      // the 16 certified SlabNecklaces k = 4 keys sit in the {c,e,h,p} census, pairwise distinct
      val slabKeys = KSlabId.classesOf(4).map { nc =>
        val (_, sps) = KSlabId.kSetOf(nc)
        (sps.sorted, canonicalKey(KSlabId.deriveClass(nc)))
      }
      slabKeys should have size 16
      slabKeys.map(_._2).distinct should have size 16
      for (sps, key) <- slabKeys do
        withClue(s"slab key on ${quadLabel(sps)}: ")(out.get(sps).keys should contain(key))
