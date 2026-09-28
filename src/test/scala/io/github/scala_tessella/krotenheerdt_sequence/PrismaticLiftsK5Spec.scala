package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** k = 5, LIFT SIDE (as `PrismaticLiftsK4Spec`): THE PRISMATIC-LIFT QUINTUPLES — the planar 5-uniform
  * Krötenheerdt atlas (A068600 n = 5 = 15 tilings, re-enumerated by the 2D SAT assembler) lifts tiling by
  * tiling through the k-role identification (`PrismaticLiftId.identifyK`): each 2D tiling must identify a
  * UNIQUE species quintuple, the 15 derived keys must be DISTINCT, and every identified quintuple must be
  * FAIR (`KSetShell`). With the k = 5 SLAB inventory EMPTY (the complete necklace census is 6/20/24/16/0 by k
  * — `SlabWorldK5Spec`), these 15 lifts are the ENTIRE k = 5 known-answer floor: N₅(3D) ≥ 15. Guarded
  * (-Dlifts.k5).
  */
class PrismaticLiftsK5Spec extends AnyFlatSpec with Matchers:

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit =
    println(s"[lift5 ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()

  "THE PRISMATIC-LIFT QUINTUPLE GATE" should
    "identify all 15 planar 5-uniform lifts uniquely, distinct, on fair quintuples (-Dlifts.k5)" in:
      assume(sys.props.contains("lifts.k5"), "15 lifts — enable with -Dlifts.k5")
      val tilings = PrismaticLiftId.theN(5)
      tilings should have size 15
      val flags   = MonoShell.Flags()
      val keys    = tilings.map { (name, ds) =>
        say(s"lift $name")
        val (types, hits) = PrismaticLiftId.identifyK(ds, 5)
        withClue(s"$name: exactly five vertex types: ")(types should have size 5)
        withClue(s"$name: unique species identification, got ${hits.map(_._1)}: ")(hits should have size 1)
        val (sps, key)    = hits.head
        val lbl           = sps.map(SpeciesCorona.label).mkString(" ~ ")
        say(s"  -> $lbl")
        withClue(s"$name -> $lbl: ")(KSetShell.fairKSet(sps.sorted, flags).fair shouldBe true)
        (name, sps, key)
      }
      keys.map(_._3).distinct should have size 15
      val byQuint = keys.groupBy(_._2.sorted).view.mapValues(_.size).toMap
      say(s"distinct species quintuples: ${byQuint.keySet.size}")
      flags.items.distinct shouldBe empty
