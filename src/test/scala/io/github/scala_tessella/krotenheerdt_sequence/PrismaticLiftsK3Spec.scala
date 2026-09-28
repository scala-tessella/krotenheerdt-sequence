package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.DelaneySymbols

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** k = 3, LIFT SIDE: THE PRISMATIC-LIFT TRIPLES — the planar 3-uniform Krötenheerdt atlas (A068600 n = 3 = 39
  * tilings, re-enumerated by the 2D SAT assembler) lifts tiling by tiling through the k-role identification
  * (`PrismaticLiftId.identifyK`): each 2D tiling must identify a UNIQUE species triple, the 39 derived keys
  * must be DISTINCT, and every identified triple must be FAIR (`KSetShell`). Census membership per triple is
  * pinned by `K3LiftIdentificationSpec`.
  *
  * The canary runs one n = 3 lift end to end; the full 39-tiling check is guarded (-Dlifts.k3).
  */
class PrismaticLiftsK3Spec extends AnyFlatSpec with Matchers:

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit =
    println(s"[lift3 ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()

  /** Identify one tiling: assert the identification is unique and return (species triple, key). */
  private def uniqueLift(name: String, ds: DelaneySymbols.DSymbol): (Vector[Int], Vector[Int]) =
    val (types, hits) = PrismaticLiftId.identifyK(ds, 3)
    withClue(s"$name: exactly three vertex types: ")(types should have size 3)
    withClue(s"$name: unique species identification, got ${hits.map(_._1)}: ")(hits should have size 1)
    hits.head

  "one prismatic-lift triple" should "identify uniquely and be a fair triple (canary, -Dlifts.k3)" in:
    assume(sys.props.contains("lifts.k3"), "runs the 2D n = 3 enumeration — enable with -Dlifts.k3")
    val (name, sym) = PrismaticLiftId.theN(3).head
    say(s"canary lift: $name")
    val (sps, _)    = uniqueLift(name, sym)
    say(s"  identified triple ${sps.map(SpeciesCorona.label).mkString(" ~ ")}")
    val flags       = MonoShell.Flags()
    KSetShell.fairKSet(sps.sorted, flags).fair shouldBe true
    flags.items.distinct shouldBe empty

  "THE PRISMATIC-LIFT TRIPLE GATE" should
    "identify all 39 planar 3-uniform lifts uniquely, distinct, on fair triples (-Dlifts.k3)" in:
      assume(sys.props.contains("lifts.k3"), "39 lifts — enable with -Dlifts.k3")
      val tilings  = PrismaticLiftId.theN(3)
      tilings should have size 39
      val flags    = MonoShell.Flags()
      val keys     = tilings.map { (name, sym) =>
        say(s"lift $name")
        val (sps, key) = uniqueLift(name, sym)
        val lbl        = sps.map(SpeciesCorona.label).mkString(" ~ ")
        say(s"  -> $lbl")
        withClue(s"$name -> $lbl: ")(KSetShell.fairKSet(sps.sorted, flags).fair shouldBe true)
        (name, sps, key)
      }
      keys.map(_._3).distinct should have size 39
      val byTriple = keys.groupBy(_._2.sorted).view.mapValues(_.size).toMap
      say(
        s"lifts per triple: ${byTriple.map((t, n) => s"${t.mkString(",")}=$n").toVector.sorted.mkString(" ")}"
      )
      say(s"distinct species triples: ${byTriple.keySet.size}")
      flags.items.distinct shouldBe empty
