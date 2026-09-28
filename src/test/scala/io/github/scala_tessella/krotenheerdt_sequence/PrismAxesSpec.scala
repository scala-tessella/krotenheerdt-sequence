package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesEnumerator
import PrismAxes.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The local claim of the two-direction theorem on the species table: every star carries at most two
  * prism-axis directions, two are perpendicular, and exactly five species are bidirectional.
  */
class PrismAxesSpec extends AnyFlatSpec with Matchers:

  "every species star" should "carry at most two prism-axis directions, perpendicular when two" in {
    val bidirectional =
      for
        i <- SpeciesEnumerator.species.indices.toVector
        t  = tableOf(i)
        _  = withClue(s"species $i")(t.axes.size should be <= 2)
        if t.axes.size == 2
      yield
        withClue(s"species $i")(t.angles.head shouldBe 90.0 +- 1e-6)
        i
    bidirectional.toSet shouldBe Set(1, 4, 20, 27, 29)
  }
