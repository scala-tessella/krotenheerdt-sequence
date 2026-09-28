package io.github.scala_tessella.krotenheerdt_sequence

import StripStacking.*
import StripSymmetry.*
import scala.jdk.CollectionConverters.*

/** THE GLOBAL ENUMERATION OF k-UNIFORM STACKINGS (the paper's section on the enumeration). Words over the
  * letters of one hexagon period are grown as segments; a segment closed on itself, and every chain s · g(s)
  * · g²(s) · … over the layer group (the only words longer than 2k by the level-orbit bound), is a candidate.
  * Candidates are filtered without a build: the species at every junction come from the cached four-layer
  * window oracle, the junction orbits from the word's own symmetries (`StripSymmetry`), and a Krötenheerdt
  * symbol with at most k species needs pairwise disjoint orbit species summing to at most k. Only survivors
  * are built and keyed by the caller. Small methods, each pinned in `StripEnumerationSpec`.
  */
object StripEnumeration:

  /** The letters of a set of hexagon periods: the cubic layer, the plain rows along u and w, and for each
    * period the rows carrying a hexagon every `period` units at each offset.
    */
  def letters(periods: Set[Int]): Vector[Layer] =
    Vector(Layer.Cubic, Layer.Tri(true, Set.empty, 1), Layer.Tri(false, Set.empty, 1)) ++
      (for p <- periods.toVector.sorted; ax <- Vector(true, false); o <- 0 until p
      yield Layer.Tri(ax, Set(o), p))

  def letters(period: Int): Vector[Layer] = letters(Set(period))

  /** The vertices per across period on the level between rows i − 1 and i of the same axis: P, the lcm of the
    * hexagon periods of the pairs meeting the level (the pair topped by row i − 1, the pair whose mid-level
    * it is, the pair started by row i), less one per hexagon of the pair whose mid-level it is (a hexagon
    * centre is not a vertex). None when the level is not between two same-axis rows or a row it needs is
    * unknown.
    */
  def levelVertices(w: Vector[Layer], i: Int, closed: Boolean): Option[Int] =
    def row(j: Int): Option[Layer] =
      if closed then Some(w(((j % w.size) + w.size) % w.size))
      else if j >= 0 && j < w.size then Some(w(j)) else None
    (row(i - 1), row(i)) match
      case (Some(Layer.Tri(a, hxMid, pMid)), Some(Layer.Tri(b, hxUp, pUp))) if a == b =>
        row(i - 2) match
          case None        => None
          case Some(below) =>
            val pBelow  =
              below match { case Layer.Tri(c, hx, per) if c == a && hx.nonEmpty => per; case _ => 1 }
            val periods = Vector(pBelow, if hxMid.nonEmpty then pMid else 1, if hxUp.nonEmpty then pUp else 1)
            val big     = periods.foldLeft(1)(lcm)
            Some(big - (if hxMid.nonEmpty then big / pMid else 0))
      case _                                                                          => None

  /** Whether level i (between layers i − 1 and i) is PLAIN: no hexagon half touches it — neither layer
    * carries hexagons of its own, and layer i − 1 carries no upper halves (it is cubic, or a row not preceded
    * by a same-axis row with hexagons). None when a layer it needs is unknown.
    */
  def plainLevel(w: Vector[Layer], i: Int, closed: Boolean): Option[Boolean] =
    def row(j: Int): Option[Layer] =
      if closed then Some(w(((j % w.size) + w.size) % w.size))
      else if j >= 0 && j < w.size then Some(w(j)) else None
    def hex(l: Layer)              = l match { case Layer.Tri(_, hx, _) => hx.nonEmpty; case _ => false }
    (row(i - 1), row(i)) match
      case (Some(below), Some(above)) if !hex(above) && !hex(below) =>
        below match
          case Layer.Cubic         => Some(true)
          case Layer.Tri(ax, _, _) =>
            row(i - 2).map {
              case Layer.Tri(a2, hx2, _) if a2 == ax && hx2.nonEmpty => false // upper halves in the row below
              case _                                                 => true
            }
      case (Some(_), Some(_))                                       => Some(false) // a hexagon half touches it
      case _                                                        => None

  /** The axis periods a plain level admits: 1, 2 and 4.
    */
  val plainPeriods: Set[Int] = Set(1, 2, 4)

  /** The paper's lemma on plain levels: at a plain level all vertices share one species, hence form one orbit
    * of the level's stabiliser. Modulo the in-plane lattice, an element keeping the axes acts on an axis's
    * residues mod L as x ↦ ±x + c, where c is 0 for a translation and satisfies 2c ≡ 0 for a glide across the
    * level, whose square is a translation; so at most four maps, two when L is odd. An axis exchange needs
    * L_u = L_w = L and at most doubles the count: 16 maps on L² residues, 8 when L is odd. One orbit then
    * forces L ∈ {1, 2, 4} on both axes. Period 4 does occur with a plain level:
    * `C Tw{1/4} Tw Tu Tw{0/4} Tw C` is 7-uniform, its cube-on-cube level one orbit under a glide by two units
    * and a reflection about a half-integer. A prefix whose axis lcm lies outside {1, 2, 4} and that has a
    * readable plain level extends to no Krötenheerdt word (the lcm only grows).
    */
  def plainLevelsOk(w: Vector[Layer], closed: Boolean): Boolean =
    val (lu, lw) = axisPeriods(w)
    if plainPeriods(lu) && plainPeriods(lw) then true
    else
      val idx = if closed then w.indices else 1 until w.size
      !idx.exists(i => plainLevel(w, i, closed).contains(true))

  /** Whether an axis lcm is 4: then every plain level needs a glide across it (the corrected plain-level
    * lemma — without one the level's elements keeping the axes act on the residues mod 4 in at most two ways,
    * and an exchange at most doubles that, too few for one orbit on 4 or 16 residues).
    */
  def needsGlide(w: Vector[Layer]): Boolean =
    val (lu, lw) = axisPeriods(w)
    lu == 4 || lw == 4

  /** The plain levels of a word (closed) or the readable ones of a prefix: junction i between layers i − 1
    * and i.
    */
  def plainLevels(w: Vector[Layer], closed: Boolean): Vector[Int] =
    val idx = if closed then w.indices else 1 until w.size
    idx.filter(i => plainLevel(w, i, closed).contains(true)).toVector

  /** The glide cut on a closed two-direction word: with an axis lcm 4, every plain level is fixed by a stack
    * reversal of the word (a junction map j ↦ −j + b fixing it). A necessary condition only — the in-plane
    * shift of that reversal is not checked. Vacuous for lifts, whose stack direction is not canonical.
    */
  def glideLevelsOk(w: Vector[Layer], group: Vector[InPlane]): Boolean =
    !twoDirection(w) || !needsGlide(w) || {
      val plain = plainLevels(w, closed = true)
      plain.isEmpty || {
        val flips = wordSymmetries(w, group).filter(_.a == -1)
        plain.forall(j => flips.exists(_(j) == j))
      }
    }

  /** The glide cut on an open two-direction prefix: a reversal fixing plain level j maps junction j + i onto
    * junction j − i and preserves species, so wherever both are readable interior junctions (2 ≤ j ± i ≤ size
    * − 2) they carry the same species. `species(i)` is junction i + 2, as `SpeciesOracle.interior`.
    */
  def prefixGlideOk(prefix: Vector[Layer], species: Vector[Set[Int]]): Boolean =
    !twoDirection(prefix) || !needsGlide(prefix) ||
      plainLevels(prefix, closed = false).forall { j =>
        Iterator
          .from(1)
          .takeWhile(i => j - i >= 2 && j + i <= prefix.size - 2)
          .forall(i => species(j - i - 2) == species(j + i - 2))
      }

  /** Whether hexagon row i is a BASE row: its lower level carries no same-axis hexagon half — the layer below
    * is cubic, a row of the other axis, or a plain same-axis row not itself preceded by a same-axis hexagon
    * row. None when row i carries no hexagons or a layer it needs is unknown.
    */
  def baseRow(w: Vector[Layer], i: Int, closed: Boolean): Option[Boolean] =
    def row(j: Int): Option[Layer] =
      if closed then Some(w(((j % w.size) + w.size) % w.size))
      else if j >= 0 && j < w.size then Some(w(j)) else None
    row(i) match
      case Some(Layer.Tri(ax, hx, _)) if hx.nonEmpty =>
        row(i - 1).flatMap {
          case Layer.Tri(a1, hx1, _) if a1 == ax =>
            if hx1.nonEmpty then Some(false) // the mid-level of the row below
            else
              row(i - 2).map {
                case Layer.Tri(a2, hx2, _) if a2 == ax && hx2.nonEmpty =>
                  false // upper halves in the row below
                case _ => true
              }
          case _                                 => Some(true)
        }
      case _                                         => None

  /** The paper's single-period theorem: in a two-direction Krötenheerdt word the base rows of one axis all
    * carry one hexagon per period, of the same period p ≤ 4, which is the across translation period of the
    * word on that axis; so every hexagon row of the axis has a period dividing p. As a filter on a word or
    * prefix with rows of both axes: per axis, the readable base rows agree on a period at most 4 that every
    * hexagon period of the axis divides; with no readable base row the periods of the axis must still divide
    * one number at most 4 (all equal, or within {2, 4}). One-direction words are lifts and are not
    * constrained.
    */
  def axisPeriodsOk(w: Vector[Layer], closed: Boolean): Boolean =
    val axes = w.collect { case Layer.Tri(ax, _, _) => ax }.toSet
    axes.size < 2 || Vector(true, false).forall { ax =>
      val rows    = w.indices.filter(i =>
        w(i) match { case Layer.Tri(a, hx, _) => a == ax && hx.nonEmpty; case _ => false }
      )
      val periods = rows.map(i => w(i).asInstanceOf[Layer.Tri].period)
      val bases   =
        rows.filter(i => baseRow(w, i, closed).contains(true)).map(i => w(i).asInstanceOf[Layer.Tri].period)
      bases.distinct.size <= 1 &&
      (bases.headOption match
        case Some(p) => p <= 4 && periods.forall(p % _ == 0)
        case None    => periods.forall(_ <= 4) && (periods.distinct.size <= 1 || periods.forall(Set(2, 4))))
    }

  /** The paper's theorem on period-2 words: a two-direction Krötenheerdt word whose hexagon periods are all
    * at most 2 has at most seven species. So for k ≥ 8 a closed word with both axis lcms at most 2 is cut,
    * and so is a prefix with readable base rows of period 2 on both axes (by the single-period lemma every
    * hexagon period of each axis then divides 2). A plain level alone is no longer a cut: it admits period 4
    * (`plainLevelsOk`). Vacuous for k ≤ 7.
    */
  def highKOk(w: Vector[Layer], k: Int, closed: Boolean): Boolean =
    k <= 7 || {
      def basePeriodTwo(ax: Boolean) = w.indices.exists(i =>
        w(i) match
          case Layer.Tri(a, hx, 2) if a == ax && hx.nonEmpty => baseRow(w, i, closed).contains(true)
          case _                                             => false
      )
      val (lu, lw)                   = axisPeriods(w)
      !(basePeriodTwo(true) && basePeriodTwo(false)) && !(closed && lu <= 2 && lw <= 2)
    }

  /** The lemma on period-4 runs (certified by `StripCertificatesSpec`, "the period-4 runs"): in a
    * two-direction Krötenheerdt word no three consecutive rows of one axis are hexagon rows of period 4 — the
    * level over the second carries species 19 with a set other than the {19, 28} of the level between the
    * first two, and 19 is one orbit. A two-direction prefix (or closed word) with such a triple extends to no
    * Krötenheerdt word. Valid at every k; vacuous for lifts.
    */
  def runs4Ok(w: Vector[Layer], closed: Boolean): Boolean =
    def hex4(l: Layer): Option[Boolean] = l match
      case Layer.Tri(ax, hx, 4) if hx.nonEmpty => Some(ax)
      case _                                   => None
    val n                               = w.size
    val starts                          = if closed then 0 until n else 0 until n - 2
    !twoDirection(w) || !starts.exists { i =>
      val a = hex4(w(i))
      a.isDefined && hex4(w((i + 1) % n)) == a && hex4(w((i + 2) % n)) == a
    }

  /** At most 2k vertex orbits lie on a level of a k-uniform word, and the vertices of one across period of
    * the level fall into at least half as many orbits (the paper's lemma on levels): every readable level of
    * the prefix has at most 2k vertices per period.
    */
  def levelsOk(w: Vector[Layer], k: Int, closed: Boolean): Boolean =
    val idx = if closed then w.indices else 2 until w.size
    idx.forall(i => levelVertices(w, i, closed).forall(_ <= 2 * k))

  /** The four-layer window oracle with a cache: the species at the interface between window(1) and window(2),
    * −1 marking an interface the oracle cannot read.
    */
  final class SpeciesOracle:
    private val cache                       = java.util.concurrent.ConcurrentHashMap[Vector[Layer], Set[Int]]()
    def at(window: Vector[Layer]): Set[Int] =
      cache.computeIfAbsent(window, w => interfaceSpecies(w).getOrElse(1, Set(-1)))

    /** The species at junction j of a closed word (between layers j − 1 and j, cyclically). */
    def junction(w: Vector[Layer], j: Int): Set[Int] =
      at(Vector(rotate(w, j - 2).head, rotate(w, j - 1).head, w(j % w.size), rotate(w, j + 1).head))

    /** The species at the interior junctions of an open prefix: junction j for 2 ≤ j ≤ size − 2. */
    def interior(prefix: Vector[Layer]): Vector[Set[Int]] = (2 to prefix.size - 2).toVector.map(j =>
      at(prefix.slice(j - 2, j + 2))
    )
    def size: Int                                         = cache.size

  /** A row carrying hexagons is followed by a same-axis row with compatible merges; the pair is checked
    * padded with a plain same-axis row so that a hexagon row may follow a hexagon row (the 6³ block).
    */
  def prefixConsistent(w: Vector[Layer]): Boolean = (0 until w.size - 1).forall { i =>
    w(i) match
      case Layer.Tri(ax, hx, _) if hx.nonEmpty =>
        w(i + 1) match
          case Layer.Tri(ax2, _, _) =>
            ax2 == ax && consistent(Vector(w(i), w(i + 1), Layer.Tri(ax, Set.empty, 1), Layer.Cubic))
          case _                    => false
      case _                                   => true
  }

  /** The junction-orbit constraints on an open prefix, from the species at its readable interior junctions
    * (junction j between layers j − 1 and j, for 2 ≤ j ≤ size − 2). For a k-uniform word take the symmetry
    * translation of least period t₀ and its segment: the junction positions of the segment are distinct
    * modulo t₀, the reflections of the word pair positions as j ↔ c − j for one centre c, and every species
    * lies in one junction orbit. Hence, within the segment: at most k distinct species and at most k distinct
    * junction profiles; a species at no more than two positions; every two positions sharing a species have
    * the same sum, so two such pairs whose sums differ by less than the prefix length (hence by less than t₀)
    * are incompatible. A prefix violating any of these extends to no k-uniform word from its own segment.
    */
  def prefixOrbitsOk(prefix: Vector[Layer], k: Int, oracle: SpeciesOracle): Boolean =
    val sp = oracle.interior(prefix) // index i is junction i + 2
    if sp.exists(_.contains(-1)) then false
    else if sp.foldLeft(Set.empty[Int])(_ ++ _).size > k then false
    else if sp.distinct.size > k then false
    else
      val positions = collection.mutable.Map.empty[Int, List[Int]]
      for (set, i) <- sp.zipWithIndex; x <- set do positions(x) = (i + 2) :: positions.getOrElse(x, Nil)
      if positions.values.exists(_.size > 2) then false
      else
        val sums = positions.values.collect { case a :: b :: Nil => a + b }.toVector.distinct.sorted
        sums.indices.forall(i => i == 0 || sums(i) - sums(i - 1) >= prefix.size)

  /** THE BLOCK CONDITION (the paper's section on the search): in a two-direction Krötenheerdt word two
    * junctions whose species sets meet carry the same set — a shared species is one vertex orbit, so the two
    * junctions lie in one junction orbit, whose junctions all carry one set. The sets of the junctions are
    * therefore pairwise equal or disjoint.
    */
  def blocksOk(sets: Iterable[Set[Int]]): Boolean =
    val distinct = sets.toVector.distinct
    distinct.indices.forall(i => (i + 1 until distinct.size).forall(j => !distinct(i).exists(distinct(j))))

  /** An open prefix may still extend to a k-uniform word: every interior junction is readable, the
    * junction-orbit constraints hold, the glide cut does, and — for a two-direction prefix — the block
    * condition on its readable junctions.
    */
  def prefixOk(prefix: Vector[Layer], k: Int, oracle: SpeciesOracle): Boolean =
    val sp = oracle.interior(prefix)
    prefixOrbitsOk(prefix, k, oracle) && prefixGlideOk(prefix, sp) && (!twoDirection(prefix) || blocksOk(sp))

  /** The species at every junction of a closed word, as a union: None if a junction is unreadable or the
    * species number more than k. The cheapest test, applied first (cached lookups only).
    */
  def speciesUnion(w: Vector[Layer], k: Int, oracle: SpeciesOracle): Option[Set[Int]] =
    var acc = Set.empty[Int]
    var j   = 0
    while j < w.size && acc.size <= k && !acc.contains(-1) do
      acc = acc ++ oracle.junction(w, j)
      j += 1
    if acc.size <= k && !acc.contains(-1) then Some(acc) else None

  /** The cheap filter of a closed word: consistent merges, readable junctions with at most k species, then —
    * for a two-direction word — pairwise disjoint orbit species summing to at most k (the orbit lower bound).
    * Returns the species per junction orbit (one set for a lift, whose symmetries need not preserve the
    * stack).
    */
  def closedOk(
      w: Vector[Layer],
      k: Int,
      oracle: SpeciesOracle,
      group: Vector[InPlane]
  ): Option[Vector[Set[Int]]] =
    if !consistent(w) || !plainLevelsOk(w, closed = true) || !axisPeriodsOk(w, closed = true) ||
      !highKOk(w, k, closed = true) || !runs4Ok(w, closed = true)
    then None
    else
      speciesUnion(w, k, oracle).flatMap { all =>
        if !twoDirection(w) then Some(Vector(all))
        else
          orbitSpecies(w, group, j => oracle.junction(w, j))
            .filter(_.map(_.size).sum <= k)
            .filter(_ => glideLevelsOk(w, group) && blocksOk(w.indices.map(j => oracle.junction(w, j))))
      }

  final case class Stats(nodes: Long, candidates: Long, survivors: Long)

  /** The search: segments over the letters of the period, grown with the prefix filters, one segment per
    * in-plane symmetry class (the chains and the extensions of a segment's image are the images of its own);
    * at every segment the closed segment and all chains over the in-plane group are candidates; a candidate
    * passes the species union, then the orbit bound, then the canonical label (one build per label) and is
    * handed to `onWord`. With `threads` > 1 the search runs on a fork-join pool, every segment's children
    * forked as subtasks, the deduplication sets being shared and concurrent, so a segment class is explored
    * by whichever task reaches it first; `onWord` must then be thread-safe. `rootLen` is kept for the
    * signature and unused.
    */
  def enumerate(
      k: Int,
      periods: Set[Int],
      maxSeg: Int,
      maxLen: Int,
      oracle: SpeciesOracle,
      onWord: (Vector[Layer], Vector[Set[Int]]) => Unit,
      threads: Int,
      rootLen: Int,
      progress: Option[String => Unit]
  ): Stats =
    val ls                                = letters(periods)
    val seen                              = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    val seenSeg                           = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    val nodes                             = java.util.concurrent.atomic.AtomicLong()
    val candidates                        = java.util.concurrent.atomic.AtomicLong()
    val survivors                         = java.util.concurrent.atomic.AtomicLong()
    val t0                                = System.currentTimeMillis
    val lastReport                        = java.util.concurrent.atomic.AtomicLong(t0)
    def report(): Unit                    = progress.foreach { say =>
      val now = System.currentTimeMillis
      val was = lastReport.get
      if now - was >= 60000 && lastReport.compareAndSet(was, now) then
        say(
          f"[${(now - t0) / 1000}%ds] segments ${nodes.get}%d candidates ${candidates.get}%d survivors ${survivors.get}%d"
        )
    }
    def candidate(w: Vector[Layer]): Unit =
      candidates.incrementAndGet()
      if (candidates.get & 0xfff) == 0 then report()
      // the species union first (cached lookups), then the orbit bound, the canonical label only for the few
      // words that pass both, the build only for a label not seen; the group is the word's own
      if consistent(w) && speciesUnion(w, k, oracle).isDefined then
        val group = effectiveGroup(w)
        closedOk(w, k, oracle, group).foreach { sp =>
          if seen.add(canonicalLayers(w, group)) then
            survivors.incrementAndGet()
            onWord(w, sp)
        }

    /** The search below a segment: with a pool, the children of a segment are forked as subtasks (a fork-join
      * recursion balances the load whatever the alphabet: with many letters the segment dedup leaves few,
      * uneven prefix classes, so a fixed-depth partition starves the pool).
      */
    def dfs(s: Vector[Layer], pool: Option[java.util.concurrent.ForkJoinPool]): Unit =
      if !levelsOk(s, k, closed = false) || !plainLevelsOk(s, closed = false) ||
        !axisPeriodsOk(s, closed = false) || !highKOk(s, k, closed = false) || !runs4Ok(s, closed = false)
      then
        return
      val group = effectiveGroup(s)
      // one segment per in-plane symmetry class: the chains and the extensions of g(s) are the images of those of s
      if !seenSeg.add(canonicalSegment(s, group)) then return
      nodes.incrementAndGet()
      if s.size >= 4 && !prefixOk(s, k, oracle) then return
      candidate(s)
      for g <- group; c <- chains(s, g, maxLen) do candidate(c) // the identity chains too: the grid drifts
      if s.size < maxSeg then
        val next = ls.map(l => s :+ l).filter(prefixConsistent)
        pool match
          case None    => next.foreach(dfs(_, None))
          case Some(p) =>
            val tasks =
              next.map(c => new java.util.concurrent.RecursiveAction { def compute(): Unit = dfs(c, pool) })
            java.util.concurrent.ForkJoinTask.invokeAll(tasks.asJava)
    def startsAlongU(l: Layer): Boolean                                              = l match
      case Layer.Tri(false, _, _) => false
      case _                      => true
    val starts                                                                       = ls.filter(startsAlongU).map(Vector(_))
    if threads <= 1 then starts.foreach(dfs(_, None))
    else
      val pool = java.util.concurrent.ForkJoinPool(threads)
      val root = new java.util.concurrent.RecursiveAction:
        def compute(): Unit =
          java.util.concurrent.ForkJoinTask.invokeAll(
            starts.map(st =>
              new java.util.concurrent.RecursiveAction { def compute(): Unit = dfs(st, Some(pool)) }
            ).asJava
          )
      pool.invoke(root)
      pool.shutdown()
    Stats(nodes.get, candidates.get, survivors.get)

  /** The search over one hexagon period. */
  def enumerate(
      k: Int,
      period: Int,
      maxSeg: Int,
      maxLen: Int,
      oracle: SpeciesOracle,
      onWord: (Vector[Layer], Vector[Set[Int]]) => Unit,
      threads: Int = 1,
      rootLen: Int = 3
  ): Stats = enumerate(k, Set(period), maxSeg, maxLen, oracle, onWord, threads, rootLen, None)
