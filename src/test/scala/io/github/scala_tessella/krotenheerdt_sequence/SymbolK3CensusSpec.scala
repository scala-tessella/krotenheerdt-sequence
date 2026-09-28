package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona
import io.github.scala_tessella.research_core.StarFoldings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.SpeciesEnumerator.species
import io.github.scala_tessella.research_core.SymbolCatalog.*
import SymbolRealizationFilter.realizeK

/** the census k = 3 pinning (transposing `SymbolK2CensusSpec`): THE k = 3 CENSUS IS EXACTLY 119 OVER THE 103
  * FAIR TRIPLES, ALL REALIZED, EVERY TAIL EMPTY — the theorem-grade battery, guarded (-Dcensus.k3: the
  * KSetShell fair-triple sweep + the scoped census at 128 + realization + every beyond-scope tail under the
  * k-part deck-conjugation lex-leader, plus the 24 certified slab keys located in their triples' censuses;
  * the 39 prismatic-lift keys are identified with their planar tilings — together 119 = 24 slab + 39 lift +
  * 56 NEW). Scope is 128 (not the sizing 96): the tail sweep found EXACTLY TWO beyond-96 minimal symbols in
  * the whole row — the 108-chamber pair on {cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3, both realized —
  * so at 128 they are census rows and every tail is empty. The unguarded canary re-derives the headline
  * triple end to end in ~1 min: the quarter-cubic interface triple carries EXACTLY TWO k = 3 classes, both
  * realized, tail empty.
  */
class SymbolK3CensusSpec extends AnyFlatSpec with Matchers:

  private lazy val flags = MonoShell.Flags()

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit = synchronized {
    println(s"[k3census ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()
  }

  private def tripleLabel(sps: Vector[Int]): String =
    sps.map(SpeciesCorona.label).mkString(" ~ ")

  /** The census, pinned triple by triple (the scoped census record). */
  private val expected: Map[String, Int] = Map(
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2"        -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2"         -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#2"                    -> 1,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"          -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3"                 -> 1,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"               -> 0,
    "{cube:2 p6:2 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"               -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2"               -> 1,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2"                -> 1,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2"                     -> 0,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2"                -> 1,
    "{p3:2 p12:4}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2"                     -> 0,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                                     -> 2,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:8 p6:2}#2"                                     -> 1,
    "{p6:6}#1 ~ {p3:4 p6:4}#1 ~ {p3:12}#2"                                         -> 1,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2"         -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1"              -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2"              -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:2 p3:4 p12:2}#2 ~ {p3:12}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2"          -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"               -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                          -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"               -> 1,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:4 p6:4}#1 ~ {p3:12}#2"                            -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                    -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                      -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                          -> 1,
    "{cube:2 p3:4 p12:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                          -> 1,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                            -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                -> 0,
    "{cube:2 p3:4 p12:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                     -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:8}#1"                     -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {p3:4 p6:4}#3"                  -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1"                -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2"                -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#3"                            -> 8,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {cube:4 p3:6}#2"                          -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3"                         -> 4,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#1"                       -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#1 ~ {cube:4 p3:6}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                         -> 6,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#1"                       -> 2,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {cube:4 p3:6}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                         -> 4,
    "{cube:4 p3:2 p6:2}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                         -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                     -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 1,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                           -> 1,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:8}#1 ~ {cube:4 p3:6}#1"                          -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                     -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                       -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                           -> 0,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                           -> 1,
    "{cube:4 p3:2 p6:2}#2 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                           -> 2,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {cube:4 p3:6}#2"                               -> 1,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                                 -> 0,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#1"                                     -> 8,
    "{cube:8}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                                     -> 8,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#2 ~ {p3:4 p6:4}#3"                                -> 2,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                                -> 2,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                                -> 0,
    "{p3:4 p6:4}#1 ~ {p3:4 p6:4}#3 ~ {p3:12}#2"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                              -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#1 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                                  -> 0,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                    -> 1,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                    -> 3,
    "{p3:4 p6:4}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 0,
    "{p3:4 p6:4}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                             -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#1"                                -> 0,
    "{p3:4 p6:4}#2 ~ {p3:4 p6:4}#3 ~ {p3:8 p6:2}#2"                                -> 0,
    "{p3:4 p6:4}#3 ~ {tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#2"        -> 4,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#1"                              -> 2,
    "{p3:4 p6:4}#3 ~ {cube:4 p3:6}#1 ~ {p3:8 p6:2}#2"                              -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:8 p6:2}#2"                                -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#1 ~ {p3:12}#2"                                    -> 4,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 0,
    "{p3:4 p6:4}#3 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                    -> 3,
    "{tet:1 truncTet:3 p3:2 p6:2}#1 ~ {tet:2 truncTet:6}#1 ~ {tet:2 truncTet:6}#2" -> 2,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:8 p6:2}#2"                            -> 1,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#1"                                -> 0,
    "{cube:4 p3:6}#1 ~ {cube:4 p3:6}#2 ~ {p3:12}#2"                                -> 1,
    "{cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                  -> 0,
    "{cube:4 p3:6}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                  -> 0,
    "{cube:4 p3:6}#1 ~ {p3:12}#1 ~ {p3:12}#2"                                      -> 8,
    "{cube:4 p3:6}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                           -> 0,
    "{cube:4 p3:6}#2 ~ {p3:8 p6:2}#2 ~ {p3:12}#2"                                  -> 0,
    "{cube:4 p3:6}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                      -> 0,
    "{cube:4 p3:6}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                           -> 0,
    "{p3:8 p6:2}#1 ~ {p3:8 p6:2}#2 ~ {p3:12}#1"                                    -> 1,
    "{p3:8 p6:2}#2 ~ {p3:12}#1 ~ {p3:12}#2"                                        -> 0,
    "{p3:8 p6:2}#2 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                             -> 0,
    "{p3:12}#1 ~ {p3:12}#2 ~ {tet:4 oct:3 p3:6}#1"                                 -> 0,
    "{p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1"                           -> 8,
    "{p3:12}#2 ~ {tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#2"                           -> 8,
    "{tet:4 oct:3 p3:6}#1 ~ {tet:8 oct:6}#1 ~ {tet:8 oct:6}#2"                     -> 8
  )

  private val scope = 128

  /** Census + realization + tail of one fair triple, everything the theorem needs, as data. */
  final private case class Outcome(
      sps: Vector[Int],
      capped: Boolean,
      keys: Set[Vector[Int]],
      count: Int,
      unrealized: Int,
      certBad: Int,
      tailSymbols: Int,
      tailCapped: Boolean
  )

  private def outcomeOf(sps: Vector[Int], log: String => Unit): Outcome =
    val (entries, stats) = kEntries(sps, maxChambers = scope, sigma0Cap = 100000)
    val verdicts         = entries.map(e => realizeK(e, flags, log = log))
    val tail             = CensusTails.runTriple(sps, scope, log, writeCert = false)
    Outcome(
      sps,
      stats.exists(_.capped),
      entries.map(e => canonicalKey(e.sym)).toSet,
      entries.size,
      verdicts.count(!_.realized),
      verdicts.count(v => v.realized && !v.cert.exists(_.ok)),
      tail.symbols.size,
      tail.capped
    )

  "the k = 3 census canary" should
    "pin the quarter-cubic interface triple end to end: EXACTLY TWO classes, both realized, tail empty" in:
      def bySupport(sup: String) =
        species.indices.toVector.filter(i => species(i).showSupport == sup)
      val q                      = bySupport("{tet:1 truncTet:3 p3:2 p6:2}")
      val tt                     = bySupport("{tet:2 truncTet:6}")
      val sps                    = (q ++ tt).sorted
      sps should have size 3
      val o                      = outcomeOf(sps, say)
      o.capped shouldBe false
      o.count shouldBe 2
      o.unrealized shouldBe 0
      o.certBad shouldBe 0
      o.tailSymbols shouldBe 0
      o.tailCapped shouldBe false

  "THE k = 3 CENSUS" should
    "be EXACTLY 119 over the 103 fair triples, per-triple pinned, all realized, all tails empty, " +
    "the 24 slab keys located (enable with -Dcensus.k3)" in:
      assume(sys.props.contains("census.k3"), "multi-hour full k = 3 battery — enable with -Dcensus.k3")
      say("deriving the fair triples (the KSetShell sweep, ~5 min)...")
      val triples = KSetShell.fairKSets(3, flags).filter(_.fair).map(_.kSet)
      triples should have size 103
      triples.map(tripleLabel).toSet shouldBe expected.keySet

      // the per-triple work runs on a fixed pool (-Dcensus.par, default 8); all assertions on the main thread
      val par  = sys.props.get("census.par").map(_.toInt).getOrElse(8).max(1)
      for i <- triples.flatten.distinct.sorted do
        StarFoldings.subgroupsOfSpecies(i) // pre-warm shared caches sequentially
      val out  = new java.util.concurrent.ConcurrentHashMap[Vector[Int], Outcome]
      val errs = new java.util.concurrent.ConcurrentHashMap[Vector[Int], String]
      val pool = java.util.concurrent.Executors.newFixedThreadPool(par)
      val gate = new java.util.concurrent.CountDownLatch(triples.size)
      for sps <- triples do
        pool.submit(new Runnable:
          def run(): Unit =
            try
              out.put(sps, outcomeOf(sps, _ => ()))
              say(s"done ${tripleLabel(sps)} (${out.size}/${triples.size})")
            catch case e: Throwable => errs.put(sps, e.toString)
            finally gate.countDown())
      gate.await()
      pool.shutdown()
      errs shouldBe empty

      var total = 0
      for sps <- triples do
        val lbl = tripleLabel(sps)
        val o   = out.get(sps)
        withClue(s"$lbl: "):
          o.capped shouldBe false
          o.count shouldBe expected(lbl)
          o.unrealized shouldBe 0
          o.certBad shouldBe 0
          o.tailSymbols shouldBe 0
          o.tailCapped shouldBe false
        total += o.count
      total shouldBe 119

      // the 24 certified SlabNecklaces keys sit in their triples' censuses, pairwise distinct
      val slabKeys = K3SlabId.k3Classes.map { nc =>
        val (_, sps) = K3SlabId.tripleOf(nc)
        (sps.sorted, canonicalKey(K3SlabId.deriveClass(nc)))
      }
      slabKeys should have size 24
      slabKeys.map(_._2).distinct should have size 24
      for (sps, key) <- slabKeys do
        withClue(s"slab key on ${tripleLabel(sps)}: ")(out.get(sps).keys should contain(key))
