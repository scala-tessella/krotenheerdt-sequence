package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The planar Krötenheerdt sequence T_n, re-derived: the planar n-uniform Krötenheerdt tilings enumerated by
  * research-core's SAT assembler (`PrismaticLiftId.theN`), counted against Krötenheerdt's 11, 20, 39, 33, 15,
  * 10, 7. Their prismatic lifts are the lift classes of the three-dimensional sequence, and T_n is the lift
  * column of the paper's table. The rows n = 1 and 2 run in the fast tier; n = 3 to 7 are opt-in (`-Dplanar`,
  * or `-Dplanar=3,5` for chosen rows). The vanishing at n = 8 is Krötenheerdt's, certified in the planar
  * paper, and is not re-run here.
  */
class PlanarSequenceSpec extends AnyFlatSpec with Matchers:

  private val krotenheerdt = Vector(11, 20, 39, 33, 15, 10, 7)

  "the planar sequence" should "count 11 and 20 tilings at n = 1 and 2" in:
    PrismaticLiftId.theN(1).size shouldBe 11
    PrismaticLiftId.theN(2).size shouldBe 20

  it should "count Krötenheerdt's T_n at n = 3 to 7 (-Dplanar)" in:
    assume(OptIn.enabled("planar"))
    val rows = sys.props("planar").split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector match
      case Vector() => (3 to 7).toVector
      case chosen   => chosen
    for n <- rows do withClue(s"n = $n: ")(PrismaticLiftId.theN(n).size shouldBe krotenheerdt(n - 1))
