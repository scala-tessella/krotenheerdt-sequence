package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SymbolCatalog

import BarlowFixtures.{developmentMatchesCloud, directPattern, germDomainsOf}
import PairPatterns.ctxOf
import io.github.scala_tessella.research_core.SymbolCatalog.Sym

/** Shared k-generic slab-class identification (the k = 4 generalization of `K3SlabId`, which now delegates
  * here): the certified derivation of a SlabNecklaces k-class's minimal k-species symbol — word cloud, k-role
  * germ anchors, direct pattern, development-vs-cloud verification from ALL k roles, `derivedPairSymbol`.
  * Fails loudly on any cloud mismatch or invalid/non-minimal symbol.
  */
object KSlabId:

  private lazy val sp = SlabFixtures.speciesByLetter

  def classesOf(k: Int): Vector[SlabNecklaces.NecklaceClass] = SlabNecklaces.classes.filter(_.k == k)

  /** The species k-set of a class, as sorted letters and species indices (the ctx role order). */
  def kSetOf(nc: SlabNecklaces.NecklaceClass): (Vector[Char], Vector[Int]) =
    val letters = nc.orbitData.speciesSet.map(_.label.head).toVector.sorted
    (letters, letters.map(sp))

  /** The certified minimal symbol of one k-class; throws on any verification failure. */
  def deriveClass(nc: SlabNecklaces.NecklaceClass): Sym =
    val (letters, sps) = kSetOf(nc)
    val ctx            = ctxOf(sps)
    val letterRole     = (ch: Char) => ctx.roleOf(sp(ch))
    val vs             = SlabFixtures.cloudOf(nc)
    val (_, anchors)   = germDomainsOf(ctx, vs, letters, letterRole)
    val pat            = directPattern(ctx, vs, anchors, letterRole)
    for r <- ctx.roles do
      if !developmentMatchesCloud(ctx, pat, vs, anchors(r), letterRole, start = r) then
        throw new IllegalStateException(s"class ${nc.showWord}: development-vs-cloud fails from role $r")
    val s              = PairRealization.derivedPairSymbol(ctx, pat)
    require(SymbolCatalog.valid(s) && SymbolCatalog.isMinimal(s), s"class ${nc.showWord}: bad symbol")
    require(s.speciesOf.distinct.toSet == sps.toSet, s"class ${nc.showWord}: species mismatch")
    s
