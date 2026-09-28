package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** k = 4, LIFT SIDE (as `PrismaticLiftsK3Spec`): THE PRISMATIC-LIFT QUADRUPLES — the planar 4-uniform
  * Krötenheerdt atlas (A068600 n = 4 = 33 tilings, re-enumerated by the 2D SAT assembler) lifts tiling by
  * tiling through the k-role identification (`PrismaticLiftId.identifyK`): each 2D tiling must identify a
  * UNIQUE species quadruple, the 33 derived keys must be DISTINCT, and every identified quadruple must be
  * FAIR (`KSetShell`). Guarded (-Dlifts.k4).
  */
class PrismaticLiftsK4Spec extends AnyFlatSpec with Matchers:

  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit =
    println(s"[lift4 ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()

  "THE PRISMATIC-LIFT QUADRUPLES" should
    "identify all 33 planar 4-uniform lifts uniquely, distinct, on fair quadruples (-Dlifts.k4)" in:
      assume(sys.props.contains("lifts.k4"), "33 lifts — enable with -Dlifts.k4")
      val tilings = PrismaticLiftId.theN(4)
      tilings should have size 33
      val flags   = MonoShell.Flags()
      val keys    = tilings.map { (name, ds) =>
        say(s"lift $name")
        val (types, hits) = PrismaticLiftId.identifyK(ds, 4)
        withClue(s"$name: exactly four vertex types: ")(types should have size 4)
        withClue(s"$name: unique species identification, got ${hits.map(_._1)}: ")(hits should have size 1)
        val (sps, key)    = hits.head
        val lbl           = sps.map(SpeciesCorona.label).mkString(" ~ ")
        say(s"  -> $lbl")
        withClue(s"$name -> $lbl: ")(KSetShell.fairKSet(sps.sorted, flags).fair shouldBe true)
        (name, sps, key)
      }
      keys.map(_._3).distinct should have size 33
      val byQuad  = keys.groupBy(_._2.sorted).view.mapValues(_.size).toMap
      say(s"distinct species quadruples: ${byQuad.keySet.size}")
      flags.items.distinct shouldBe empty
