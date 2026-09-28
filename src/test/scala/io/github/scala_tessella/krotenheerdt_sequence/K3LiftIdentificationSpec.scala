package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesCorona

import io.github.scala_tessella.research_core.StarFoldings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.SymbolCatalog.{canonicalKey, kEntries}

/** k = 3 identification, LIFT SIDE: all 39 prismatic-lift keys (A068600 n = 3, re-enumerated by the 2D SAT
  * assembler, identified uniquely by `PrismaticLiftId.identifyK`) SIT IN THEIR TRIPLES' SCOPED CENSUSES —
  * with the core battery (`SymbolK3CensusSpec`, census 119 with the 24 slab keys located) this pins the split
  * 119 = 24 slab + 39 lift + 56 NEW. Guarded (-Dcensus.k3; the 26 lift triples re-censused at scope 128,
  * parallel via -Dcensus.par).
  */
class K3LiftIdentificationSpec extends AnyFlatSpec with Matchers:

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit = synchronized {
    println(s"[k3lift ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()
  }

  private val scope = 128

  "THE 39 PRISMATIC-LIFT KEYS" should
    "each sit in their triple's scoped k = 3 census (enable with -Dcensus.k3)" in:
      assume(sys.props.contains("census.k3"), "26 triple censuses — enable with -Dcensus.k3")
      val tilings = PrismaticLiftId.theN(3)
      tilings should have size 39
      val lifts   = tilings.map { (name, ds) =>
        val (types, hits) = PrismaticLiftId.identifyK(ds, 3)
        withClue(s"$name: ")(types should have size 3)
        withClue(s"$name: unique identification: ")(hits should have size 1)
        val (sps, key)    = hits.head
        (name, sps.sorted, key)
      }
      lifts.map(_._3).distinct should have size 39
      val triples = lifts.map(_._2).distinct
      say(s"censusing the ${triples.size} lift triples at scope $scope...")

      val par  = sys.props.get("census.par").map(_.toInt).getOrElse(8).max(1)
      for i <- triples.flatten.distinct.sorted do StarFoldings.subgroupsOfSpecies(i)
      val out  = new java.util.concurrent.ConcurrentHashMap[Vector[Int], Set[Vector[Int]]]
      val errs = new java.util.concurrent.ConcurrentHashMap[Vector[Int], String]
      val pool = java.util.concurrent.Executors.newFixedThreadPool(par)
      val gate = new java.util.concurrent.CountDownLatch(triples.size)
      for sps <- triples do
        pool.submit(new Runnable:
          def run(): Unit =
            try
              val (entries, stats) = kEntries(sps, maxChambers = scope, sigma0Cap = 100000)
              if stats.exists(_.capped) then errs.put(sps, "capped census")
              out.put(sps, entries.map(e => canonicalKey(e.sym)).toSet)
              say(s"censused ${sps.map(SpeciesCorona.label).mkString(" ~ ")} (${out.size}/${triples.size})")
            catch case e: Throwable => errs.put(sps, e.toString)
            finally gate.countDown())
      gate.await()
      pool.shutdown()
      errs shouldBe empty

      for (name, sps, key) <- lifts do
        withClue(s"lift $name on ${sps.map(SpeciesCorona.label).mkString(" ~ ")}: "):
          out.get(sps) should contain(key)
