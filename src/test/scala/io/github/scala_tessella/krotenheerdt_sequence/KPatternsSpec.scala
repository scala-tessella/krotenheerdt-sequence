package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell
import io.github.scala_tessella.research_core.TransitivePatterns

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import PairPatterns.*
import PairShell.Pl
import io.github.scala_tessella.research_core.SpeciesEnumerator.species
import io.github.scala_tessella.research_core.TransitivePatterns.matOf

/** The k-ROLE GENERALIZATION of the pair machinery: `KCtx`/`KPattern` and the generalized filters/search must
  * reproduce the k = 2 machinery EXACTLY (the k = 2 checks are the regression oracle; here the fast teeth),
  * and the k-ary code paths must run for real at k = 3. Pins:
  *
  *   - the generalized per-species shell filter EQUALS the original cross/same-count check on every k = 2
  *     input (all Barlow joint skeletons, the raw domains, and the known negative);
  *   - k = 3 contexts and joint domains build on a GENUINE triple ({c, h, e} carries 8 SlabNecklaces
  *     classes), and the sound filters keep every slot alive there — a genuine k-set's full domains can never
  *     be emptied by sound filtering;
  *   - three embedded certified k = 1 germs run the whole k = 3 search pipeline (walks, R1, emission with
  *     three roles) and develop each role's certified TransitivePatterns ball exactly; requiring mixedness
  *     rejects the all-same union, exercising the k ≥ 3 mixedness/connectivity acceptance.
  */
class KPatternsSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val Vector(octetH, octetC) = // #1 h (two distinct figures), #2 c (single figure)
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse
  private lazy val elongated              = bySupport("{tet:4 oct:3 p3:6}").head
  private lazy val cubic                  = bySupport("{cube:8}").head

  private def vAdd(a: MonoShell.Vec, b: MonoShell.Vec): MonoShell.Vec =
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  private def vNorm(a: MonoShell.Vec): Double                         =
    math.sqrt(a._1 * a._1 + a._2 * a._2 + a._3 * a._3)
  private def vSub(a: MonoShell.Vec, b: MonoShell.Vec): MonoShell.Vec =
    (a._1 - b._1, a._2 - b._2, a._3 - b._3)

  /** The ORIGINAL k = 2 species-count shell check, verbatim (cross/same counts) — the equivalence oracle for
    * the per-species generalization.
    */
  private def speciesShellOkOriginal(ctx: KCtx, doms: Vector[Vector[Vector[Pl]]]): Boolean =
    !doms.forall(_.forall(d => d.nonEmpty && d.forall(_.sp == d.head.sp))) || {
      val spSlot     = Vector(0, 1).map(r => doms(r).map(_.head.sp))
      val crossCount = Vector(0, 1).map(r => spSlot(r).count(_ != ctx.spIdx(r)))
      (0 to 1).forall { r =>
        val u = ctx.g(r).u
        u.indices.forall { x =>
          val rep       = doms(r)(x).head
          val sp2       = rep.sp
          val r2        = ctx.roleOf(sp2)
          val placed    = ctx.g(r2).u.map(z => vAdd(u(x), rep.glu.rot(z)))
          val visible   = (ctx.spIdx(r), (0.0, 0.0, 0.0)) +:
            u.indices.toVector.map(x2 => (spSlot(r)(x2), u(x2)))
          val nbrSp     = visible.collect {
            case (sp, pos) if placed.exists(pp => vNorm(vSub(pp, pos)) < 1e-4) => sp
          }
          val crossSeen = nbrSp.count(_ != sp2)
          crossSeen <= crossCount(r2) && (nbrSp.size - crossSeen) <= ctx.g(r2).u.size - crossCount(r2)
        }
      }
    }

  private lazy val flagsCH = MonoShell.Flags()
  private lazy val ctxCH   = ctxOf(octetC, octetH)
  private lazy val domsCH  = jointDomainsOf(ctxCH, flagsCH)

  "the k-ary context" should "reproduce the pair context at k = 2 and build at k = 3" in:
    val v = ctxOf(Vector(octetC, octetH))
    v.spIdx shouldBe ctxCH.spIdx
    v.roles shouldBe (0 until 2)
    v.g.map(_.u) shouldBe ctxCH.g.map(_.u)
    val t = ctxOf(Vector(octetC, octetH, elongated))
    t.roles shouldBe (0 until 3)
    t.roleOf(elongated) shouldBe 2
    an[IllegalArgumentException] should be thrownBy ctxOf(Vector(octetC, octetC))

  "the per-species shell filter" should "equal the original cross/same check on every Barlow skeleton" in:
    val cosets  = Vector(0, 1).map(r => domsCH(r).map(d => cosetsOf(ctxCH, d)))
    val (s0, _) = roleSkeletons(ctxCH, 0, cosets(0), flagsCH)
    val (s1, _) = roleSkeletons(ctxCH, 1, cosets(1), flagsCH)
    val joint   = for a <- s0; b <- s1 yield Vector(a, b)
    joint.size shouldBe 102
    var kept    = 0
    for skel <- joint do
      val expected = speciesShellOkOriginal(ctxCH, skel)
      withClue(s"skeleton verdicts differ: ")(speciesShellOk(ctxCH, skel) shouldBe expected)
      if expected then kept += 1
    kept should be > 0
    // the raw (species-mixed) domains stay vacuously true under both
    speciesShellOk(ctxCH, domsCH) shouldBe speciesShellOkOriginal(ctxCH, domsCH)
    speciesShellOk(ctxCH, domsCH) shouldBe true

  "the k = 3 joint domains" should "build on the genuine slab triple {c, h, e} and survive sound filters" in:
    val flags = MonoShell.Flags()
    val ctx   = ctxOf(Vector(octetC, octetH, elongated))
    val doms  = jointDomainsOf(ctx, flags)
    doms.size shouldBe 3
    for r <- ctx.roles; x <- doms(r).indices do
      withClue(s"role $r slot $x: ")(doms(r)(x) should not be empty)
    // every species appears somewhere in every role's domains (the substrate of a genuine triple)
    for r <- ctx.roles do
      doms(r).flatten.map(_.sp).toSet shouldBe Set(octetC, octetH, elongated)
    // heterogeneous domains: the skeleton-level filters are vacuous, exactly as at k = 2
    speciesShellOk(ctx, doms) shouldBe true
    speciesArrangementFilter(ctx, doms) shouldBe Some(doms)
    // the R1 fixpoint is sound and {c, h, e} carries genuine honeycombs (SlabNecklaces k = 3): no slot
    // may empty
    val r1    = r1ViabilityFixpoint(ctx, doms)
    r1 should not be None
    for d <- r1; r <- ctx.roles; x <- d(r).indices do
      withClue(s"role $r slot $x after R1 fixpoint: ")(d(r)(x) should not be empty)
    flags.items.distinct shouldBe empty

  "three embedded certified germs" should "pass the k = 3 search pipeline and develop each ball" in:
    val flags       = MonoShell.Flags()
    val sps         = Vector(cubic, octetC, octetH)
    val ctx         = ctxOf(sps)
    val accs        = sps.map(i => TransitivePatterns.acceptedOf(i, flags))
    val pats        = accs.map(_.patterns.head._2)
    val doms        = ctx.roles.toVector.map(r => pats(r).glus.map(glu => Vector(Pl(sps(r), glu))))
    // each filter is sound: the embedded certified germs survive untouched
    speciesShellOk(ctx, doms) shouldBe true
    r1ViabilityFixpoint(ctx, doms) shouldBe Some(doms)
    walkSupportFixpoint(ctx, doms) shouldBe Some(doms)
    // the search accepts exactly the embedding (mixedness off: these are k = 1 germs)
    val found       = collection.mutable.ArrayBuffer.empty[KPattern]
    val (n, capped) = searchPairPatterns(ctx, doms, 2, found += _, requireMixed = false)
    (n, capped) shouldBe (1, false)
    for r <- ctx.roles do
      found.head.p(r).map(_.glu) shouldBe pats(r).glus
      // the k-role development from each base reproduces TransitivePatterns' certified ball exactly (all in role r)
      val ball          = developBall(ctx, found.head, 3.05, start = r)
      ball should not be empty
      val Some(entries) = ball: @unchecked
      entries.map(_._1).distinct shouldBe Vector(r)
      val g3bBall       = TransitivePatterns.developBall(accs(r).g, pats(r), accs(r).stab, 3.05).get
      entries.map((_, t) => TransitivePatterns.round4(t.t)).toSet shouldBe
        g3bBall.map(t => TransitivePatterns.round4(t.t)).toSet
    flags.items.distinct shouldBe empty

  it should "be rejected when mixedness is required (k = 1 germs are not a k = 3 pattern)" in:
    val flags       = MonoShell.Flags()
    val sps         = Vector(cubic, octetC, octetH)
    val ctx         = ctxOf(sps)
    val doms        = ctx.roles.toVector.map { r =>
      TransitivePatterns.acceptedOf(sps(r), flags).patterns.head._2.glus.map(glu => Vector(Pl(sps(r), glu)))
    }
    val (n, capped) = searchPairPatterns(ctx, doms, 2, _ => ())
    (n, capped) shouldBe (0, false)
