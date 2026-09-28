package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.solver.SymbolAssembly

import io.github.scala_tessella.research_core.SymbolRenderer

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.Signatures

import io.github.scala_tessella.ring_seq.RingSeq.*
import BarlowFixtures.V3
import io.github.scala_tessella.research_core.DelaneySymbols.DSymbol
import io.github.scala_tessella.research_core.HoneycombAlphabet.CellType
import PairPatterns.ctxOf
import io.github.scala_tessella.research_core.Signatures.VertexSignature
import io.github.scala_tessella.research_core.SpeciesEnumerator.species
import io.github.scala_tessella.research_core.SymbolCatalog.canonicalKey
import PairRealization.derivedPairSymbol

/** Shared prismatic-lift identification — a planar 2-uniform tiling's minimal 2D Delaney symbol lifted into
  * the k = 2 census. Used by `PrismaticLiftsK2Spec` (which asserts the identification is unique and lands in
  * the census) and by the k-role checks. The 3D species pair is identified by SUPPORT-matched candidates
  * through the direct-pattern pipeline — a genuine lift survives the both-roles development-vs-cloud match
  * uniquely.
  */
object PrismaticLiftId:

  private val flags                   = MonoShell.Flags()
  private def round5(x: Double): Long = math.round(x * 1e5)

  /** Interior vertices of the developed tiling, each with its normalized cyclic face arrangement. */
  def typedVertices(ds: DSymbol, radius: Double): Vector[((Double, Double), VertexSignature)] =
    val faces    = SymbolRenderer.develop(ds, radius)
    val byVertex = collection.mutable.Map.empty[(Long, Long), List[(Int, Double, Double)]]
    val rawOf    = collection.mutable.Map.empty[(Long, Long), (Double, Double)] // raw coord behind the
    // dedup key: the fixture adjacency tests unit distances at 1e-6, so quantized positions must not leak
    for (p, corners) <- faces do
      val cx = corners.map(_.x).sum / corners.size
      val cy = corners.map(_.y).sum / corners.size
      for c <- corners do
        val k = (round5(c.x), round5(c.y))
        rawOf.getOrElseUpdate(k, (c.x, c.y))
        byVertex(k) = (p, cx, cy) :: byVertex.getOrElse(k, Nil)
    byVertex.toVector.flatMap { (k, inc) =>
      val (vx, vy) = rawOf(k)
      if math.abs(inc.map((p, _, _) => math.Pi * (p - 2) / p).sum - 2 * math.Pi) > 1e-6 then None
      else
        Some((
          (vx, vy),
          Signatures.normalize(inc.sortBy((_, cx, cy) => math.atan2(cy - vy, cx - vx)).map(_._1))
        ))
    }

  private def liftSupport(t: VertexSignature): Map[Int, Int] =
    val cellOf = Map(
      3  -> CellType.P3,
      4  -> CellType.Cube,
      6  -> CellType.P6,
      8  -> CellType.P8,
      12 -> CellType.P12
    )
    t.groupBy(identity).map((p, ps) => cellOf(p).ordinal -> 2 * ps.size)

  private[krotenheerdt_sequence] def speciesCandidates(t: VertexSignature): Vector[Int] =
    val want = liftSupport(t)
    species.indices.toVector.filter(i =>
      species(i).state.corners.groupBy(_.cell.ordinal).map((c, cs) => c -> cs.size) == want
    )

  /** The 2D types of a tiling (exactly two, ordered) and a human name for it. */
  def nameOf(ds: DSymbol, types: Vector[VertexSignature]): String =
    types.map(_.mkString(".")).mkString(" ~ ")

  /** All surviving (species tuple, canonicalKey) identifications of the n-uniform lift — exactly one for a
    * genuine lift (the lift specs assert it): per support-matched species-candidate tuple, the labeled lift
    * cloud is run through the k-role direct-pattern pipeline with the development-vs-cloud match verified
    * from EVERY role. The cloud radius ESCALATES 8.5 → 12.5 → 16.5 → 20.5 on zero hits (fires only when the
    * smaller cloud found nothing, so k ≤ 3 behavior is unchanged): a larger-unit-cell tiling clips the
    * smaller cloud and the dev-vs-cloud match fails spuriously — first seen at k = 4 on 3.3.3.3.3.3 ~
    * 3.3.3.4.4 ~ 3.4.4.6 ~ 3.6.3.6, whose true quadruple needs 16.5 ; the 20.5 rung is the k = 5 repetition
    * of the same lesson: 3.3.3.3.3.3 ~ 3.3.3.4.4 ~ 3.3.6.6 ~ 3.4.4.6 ~ 3.6.3.6 identifies to zero quintuples
    * at 16.5 (the k = 5 check caught it).
    */
  def identifyK(ds: DSymbol, k: Int): (Vector[VertexSignature], Vector[(Vector[Int], Vector[Int])]) =
    val radii = Vector(8.5, 12.5, 16.5, 20.5)
    val first = identifyKAt(ds, k, radii.head)
    radii.tail.foldLeft(first) { (acc, r) =>
      if acc._2.nonEmpty || acc._1.size != k then acc else identifyKAt(ds, k, r)
    }

  private def identifyKAt(
      ds: DSymbol,
      k: Int,
      radius: Double
  ): (Vector[VertexSignature], Vector[(Vector[Int], Vector[Int])]) =
    val tv                        = typedVertices(ds, radius)
    val types                     = tv.map(_._2).distinct.sorted(using math.Ordering.Implicits.seqOrdering)
    if types.size != k then return (types, Vector.empty)
    val letters                   = Vector.tabulate(k)(i => ('A' + i).toChar)
    val letterOf                  = types.zip(letters).toMap
    val cloud: Vector[(V3, Char)] =
      for
        ((x, y), t) <- tv
        layer       <- -5 to 5
      yield ((x, y, layer.toDouble), letterOf(t))
    val combos                    = types
      .map(speciesCandidates)
      .foldLeft(Vector(Vector.empty[Int]))((acc, cs) => for t <- acc; c <- cs yield t :+ c)
      .filter(c => c.distinct.size == k)
    val hits                      =
      for
        combo      <- combos
        (sps, key) <- scala.util.Try {
                        val ctx          = ctxOf(combo)
                        val letterRole   = (ch: Char) => letters.indexOf(ch)
                        val (_, anchors) = BarlowFixtures.germDomainsOf(ctx, cloud, letters, letterRole)
                        val pat          = BarlowFixtures.directPattern(ctx, cloud, anchors, letterRole)
                        if ctx.roles.forall(r =>
                            BarlowFixtures
                              .developmentMatchesCloud(ctx, pat, cloud, anchors(r), letterRole, start = r)
                          )
                        then Some((combo, canonicalKey(derivedPairSymbol(ctx, pat))))
                        else None
                      }.toOption.flatten
      yield (sps, key)
    (types, hits)

  /** The two-type identification, historical shape (the k = 2 call sites). */
  def identify(ds: DSymbol): (Vector[VertexSignature], Vector[(Int, Int, Vector[Int])]) =
    val (types, hits) = identifyK(ds, 2)
    (types, hits.map((sps, key) => (sps(0), sps(1), key)))

  /** The planar n-uniform Krötenheerdt tilings (A068600), deterministically ordered. */
  def theN(n: Int): Vector[(String, DSymbol)] =
    (for
      (ts, r)    <- SymbolAssembly.enumerate(n, parallelism = 8).toVector
      (key, sym) <- r.tilings.toVector
    yield (ts.toVector.map(_.mkString(".")).sorted.mkString("; ") + "  " + key, sym)).sortBy(_._1)

  /** The 20 planar 2-uniform tilings (A068600 n = 2), deterministically ordered. */
  def the20(): Vector[(String, DSymbol)] = theN(2)
