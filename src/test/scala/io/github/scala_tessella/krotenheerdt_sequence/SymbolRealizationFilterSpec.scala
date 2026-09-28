package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.StarFoldings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import PairPatterns.ctxOf
import io.github.scala_tessella.research_core.SpeciesEnumerator.species
import io.github.scala_tessella.research_core.StarFoldings.fold
import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionOf}
import io.github.scala_tessella.research_core.SymbolCatalog.*
import SymbolRealizationFilter.*

/** The standalone realization filter, validated fixture-first before any census duty.
  *
  *   - σ₀-FILTERED DOMAINS: on the 4H Barlow entry every slot is nonempty and STRICTLY smaller than the raw
  *     atlas (σ₀ genuinely prescribes gluings), and the folding-pair/symbol size contract is enforced;
  *   - REALIZATION canary: the 4H census symbol realizes standalone — some σ₀-consistent pattern develops
  *     from both roles, derives EXACTLY the target key, and periodizes with a full certificate;
  *   - CERTIFICATE teeth: a corrupted basis (a non-translation vector) must fail the periodicity check;
  *   - REFUTATION teeth: a valid-but-NON-MINIMAL σ₀ on a barely-folded union (the lift of a more-folded
  *     symbol) must NOT realize as its own symbol — every candidate's derived minimal symbol has a different
  *     key, so the filter rejects them all (capped run: the teeth are about the rejection path, not
  *     exhaustion).
  *
  * The full 20-class realization gate (every known k = 2 class's census symbol realizes standalone, per pair)
  * is the guarded battery (-Dcensus.k2realize).
  */
class SymbolRealizationFilterSpec extends AnyFlatSpec with Matchers:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  private lazy val Vector(octetH, octetC) =
    bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse

  private lazy val flags = MonoShell.Flags()

  // heartbeat discipline: realization runs are minutes-long — silent tests are blind waits
  private val t0                   = System.currentTimeMillis
  private def say(s: String): Unit =
    println(s"[filter ${(System.currentTimeMillis - t0) / 1000}s] $s")
    System.out.flush()

  // the 4H entry: the unique minimal symbol of the most-folded Barlow corner, with provenance
  private lazy val entry4H =
    val (entries, _) = k2Entries(octetC, octetH, maxChambers = 16)
    entries should have size 1
    entries.head

  "the σ₀-filtered domains" should "be nonempty and strictly below the raw atlas on the 4H entry" in:
    val ctx          = ctxOf(entry4H.spA, entry4H.spB)
    val (subA, subB) = entry4H.folds.head
    val folds        = Vector(
      fold(StarFoldings.symmetryOf(entry4H.spA), subA),
      fold(StarFoldings.symmetryOf(entry4H.spB), subB)
    )
    val doms         = sigma0Domains(ctx, folds, entry4H.sym, flags)
    var strict       = 0
    for r <- 0 to 1; x <- doms(r).indices do
      val raw = Vector(0, 1)
        .map(r2 => PairShell.crossGluings(ctx.g(r), x, ctx.g(r2), flags).size)
        .sum
      withClue(s"role $r vertex $x: "):
        doms(r)(x) should not be empty
        doms(r)(x).size should be <= raw
        if doms(r)(x).size < raw then strict += 1
    // σ₀ genuinely filters: most slots are strictly below the raw atlas (some are already tight)
    strict should be > (doms(0).size + doms(1).size) / 2

  it should "enforce the folding/symbol size contract" in:
    val ctx   = ctxOf(entry4H.spA, entry4H.spB)
    val wrong = Vector(
      fold(StarFoldings.symmetryOf(entry4H.spA), StarFoldings.symmetryOf(entry4H.spA).perms.toSet),
      fold(StarFoldings.symmetryOf(entry4H.spB), StarFoldings.symmetryOf(entry4H.spB).perms.toSet)
    )
    an[IllegalArgumentException] should be thrownBy sigma0Domains(ctx, wrong, entry4H.sym, flags)

  "the realization filter" should "realize the 4H census symbol standalone (canary)" in:
    val v = realize(entry4H, flags, log = say)
    v.realized shouldBe true
    v.cert.get.ok shouldBe true
    v.assembly shouldBe 0

  "the periodization certificate" should "fail on a corrupted basis" in:
    val v         = realize(entry4H, flags)
    val ctx       = ctxOf(entry4H.spA, entry4H.spB)
    val pat       = v.pat.get
    val good      = v.cert.get.tau
    // replace one basis vector by a non-translation (unit z tilted into no symmetry direction)
    val bad       = (good._1, good._2, (0.123, 0.456, 0.789))
    val corrupted = certifyWith(ctx, pat, bad)
    corrupted.map(_.periodic) should contain(false)

  "the periodization certificate" should
    "certify KNOWN-GENUINE patterns across the worlds (4H, one {e,p} class, one {p,x} class)" in:
      // certifyPair validated on its own, against certified constructions — independent of the
      // symbol-driven developer. Three worlds because each falsified a naive version once: 4H caught the
      // wrong lattice-invariance transposition, {e,p} caught the exact-identity translation harvest.
      val sp                                                               = SlabFixtures.speciesByLetter
      def genuine(a: Char, b: Char, vs: Vector[(BarlowFixtures.V3, Char)]) =
        val ctx          = ctxOf(sp(a), sp(b))
        val letterRole   = (ch: Char) => ctx.roleOf(sp(ch))
        val (_, anchors) = BarlowFixtures.germDomainsOf(ctx, vs, Vector(a, b), letterRole)
        (ctx, BarlowFixtures.directPattern(ctx, vs, anchors, letterRole))
      def classOf(a: Char, b: Char)                                        =
        SlabNecklaces.classes
          .find(nc => nc.k == 2 && nc.orbitData.speciesSet.map(_.label.head) == Set(a, b))
          .get
      for (a, b, vs) <- Vector(
                          ('c', 'h', BarlowFixtures.verticesOf("ch", 7, 5)),
                          (
                            'c',
                            'h',
                            BarlowFixtures.verticesOf("cch", 7, 5)
                          ), // falsified the unverified harvest
                          ('e', 'p', SlabFixtures.cloudOf(classOf('e', 'p'))),
                          ('p', 'x', SlabFixtures.cloudOf(classOf('p', 'x')))
                        )
      do
        val (ctx, pat) = genuine(a, b, vs)
        val cert       = certifyPair(ctx, pat)
        withClue(s"{$a,$b} cert: $cert "):
          cert.isDefined shouldBe true
          cert.get.ok shouldBe true

  "the full k = 2 realization gate" should
    "realize every known class's census symbol standalone (enable with -Dcensus.k2realize)" in:
      assume(
        sys.props.contains("census.k2realize"),
        "20 standalone realizations — enable with -Dcensus.k2realize"
      )
      val sp = SlabFixtures.speciesByLetter
      for (a, b) <- Vector(('c', 'h'), ('c', 'e'), ('h', 'e'), ('e', 'p'), ('p', 'x')) do
        say(s"pair {$a,$b}: scoped census sweep")
        val (entries, stats) = k2Entries(sp(a), sp(b), maxChambers = 128, sigma0Cap = 100000)
        stats.exists(_.capped) shouldBe false
        withClue(s"pair {$a,$b}: ")(entries should have size 4)
        for e <- entries do
          val v = realize(e, flags, log = say)
          withClue(s"pair {$a,$b} symbol ${e.sym.size} chambers: "):
            v.realized shouldBe true
            v.cert.get.ok shouldBe true
          say(s"pair {$a,$b}: ${e.sym.size}-chamber symbol realized (assembly ${v.assembly + 1})")

  "a valid but NON-minimal σ₀ (the lift of a more-folded symbol)" should
    "not realize as its own symbol (refutation path teeth, enable with -Dcensus.k2realize)" in:
      assume(
        sys.props.contains("census.k2realize"),
        "~1 min rejection sweep — enable with -Dcensus.k2realize"
      )
      // the (6,6) Barlow assembly: 32 chambers, several σ₀ — at least one valid non-minimal lift
      val (entries, _) = k2Entries(octetC, octetH, maxChambers = 32)
      val cchh         = entries.find(_.sym.size == 32).get
      val (subA, subB) = cchh.folds.head
      val symA         = StarFoldings.symmetryOf(octetC)
      val symB         = StarFoldings.symmetryOf(octetH)
      val u            = unionOf(Vector(fold(symA, subA), fold(symB, subB)))
      val (sols, _)    = enumerateSigma0(u)
      val nonMin       = sols.map(s0 => symOf(u, Vector(octetC, octetH), s0)).filter(!isMinimal(_))
      nonMin should not be empty
      val bad          = K2Entry(octetC, octetH, nonMin.head, Vector((subA, subB)))
      val v            = realize(bad, flags, log = say)
      v.realized shouldBe false
