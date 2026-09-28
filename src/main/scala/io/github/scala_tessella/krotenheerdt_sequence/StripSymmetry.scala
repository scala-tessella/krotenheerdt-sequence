package io.github.scala_tessella.krotenheerdt_sequence

import StripStacking.Layer

/** THE SYMMETRIES OF A STACKING WORD (the paper's proposition on the symmetries of a word). A symmetry of a
  * two-direction stacking honeycomb preserves the stack axis (the slabs' normal is canonical), so it acts on
  * the stack by a translation or a reflection and in the plane by an isometry of the square frame: the axis
  * exchange, a reflection of each across coordinate, a translation. On a cyclic word it is a rotation or a
  * reversal composed with the in-plane isometry, which acts on the rows' hexagon residues geometrically: the
  * model's grid drifts by half a unit per row of an axis, so the action of a reflection or of the drift on a
  * residue depends on the row's rank. `image` applies an isometry to a segment placed at given grid offsets;
  * everything else is built on it. A symmetry acts on the word's junctions (junction j is the vertex level
  * below layer j) as j ↦ ±j + b; the junction orbits bound the vertex orbits from below, since every species
  * of a Krötenheerdt symbol belongs to one vertex orbit, hence to one junction orbit.
  */
object StripSymmetry:

  private def mod(a: Int, n: Int): Int = ((a % n) + n) % n

  /** An in-plane isometry of the square frame: a reflection of the u-rows' across coordinate (`ru`) and of
    * the w-rows' (`rw`), translations of each in half-units (`tu2`, `tw2`, taken modulo twice the hexagon
    * period of the row they act on), then the axis exchange if `swap`.
    */
  final case class InPlane(swap: Boolean, ru: Boolean, rw: Boolean, tu2: Int, tw2: Int):
    def isIdentity: Boolean = !swap && !ru && !rw && tu2 == 0 && tw2 == 0

  /** The in-plane group for across periods lu (u-rows) and lw (w-rows), translations in half-units up to
    * twice the period: 8·(2lu)·(2lw) isometries. Half the translations fail the grid parity in any given
    * context and cost a rejected image; both parities are needed, since a segment with an odd number of rows
    * on an axis drifts by a half unit.
    */
  def inPlaneGroup(lu: Int, lw: Int): Vector[InPlane] =
    for
      sw  <- Vector(false, true); ru <- Vector(false, true); rw <- Vector(false, true)
      tu2 <- 0 until 2 * lu; tw2     <- 0 until 2 * lw
    yield InPlane(sw, ru, rw, tu2, tw2)

  /** The in-plane group of one hexagon period on both axes: 32·period² isometries. */
  def inPlaneGroup(period: Int): Vector[InPlane] = inPlaneGroup(period, period)

  /** The least common multiple of the hexagon periods of the rows of each axis (1 when none carries
    * hexagons): translations beyond it act on the word's residues like smaller ones.
    */
  def axisPeriods(w: Vector[Layer]): (Int, Int) =
    def l(ax: Boolean) =
      w.collect { case Layer.Tri(a, hx, per) if a == ax && hx.nonEmpty => per }.foldLeft(1)(StripStacking.lcm)
    (l(true), l(false))

  /** The in-plane group that matters for a word: over its own axis periods. */
  def effectiveGroup(w: Vector[Layer]): Vector[InPlane] =
    val (lu, lw) = axisPeriods(w)
    inPlaneGroup(lu, lw)

  /** The number of rows of each axis (u, w). */
  def rows(w: Vector[Layer]): (Int, Int) =
    (
      w.count { case Layer.Tri(true, _, _) => true; case _ => false },
      w.count { case Layer.Tri(false, _, _) => true; case _ => false }
    )

  /** The image of a segment whose rows start at the across offsets `start` (half-units, per axis u, w) under
    * an isometry, placed where the image rows start at the offsets `target` (per IMAGE axis). A hexagon at
    * residue m of a row of local rank ρ sits at P = start + ρ + 1 + 2m (half-units); its image is εP + t on
    * the image axis, whose row has offset target + ρ, so m′ = (εP + t − target − ρ − 1)/2. None unless every
    * hexagon lands on the image grid (a parity condition on t, the same for all rows of an axis).
    */
  def image(g: InPlane, seg: Vector[Layer], start: (Int, Int), target: (Int, Int)): Option[Vector[Layer]] =
    var ru  = 0
    var rw  = 0
    val out = Vector.newBuilder[Layer]
    var ok  = true
    for l <- seg do
      l match
        case Layer.Cubic            => out += Layer.Cubic
        case Layer.Tri(ax, hx, per) =>
          val rank             = if ax then { ru += 1; ru - 1 }
          else { rw += 1; rw - 1 }
          val (refl, t2, from) = if ax then (g.ru, g.tu2, start._1) else (g.rw, g.tw2, start._2)
          val imageAx          = if g.swap then !ax else ax
          val to               = if imageAx then target._1 else target._2
          val eps              = if refl then -1 else 1
          val base             = eps * (from + rank + 1) + t2 - to - rank - 1 // twice m′ minus 2εm
          if base % 2 != 0 && hx.nonEmpty then ok = false
          out += Layer.Tri(imageAx, hx.map(m => mod((base + 2 * eps * m) / 2, per)), per)
    if ok then Some(out.result()) else None

  /** The isometry applied to a whole word read from offset zero (a symmetry test, a canonical form). */
  def apply(g: InPlane, w: Vector[Layer]): Option[Vector[Layer]] = image(g, w, (0, 0), (0, 0))

  def rotate(w: Vector[Layer], r: Int): Vector[Layer] =
    val k = mod(r, w.size)
    w.drop(k) ++ w.take(k)

  /** The mirror image of the stack as a layer word: the layers in reverse order, every hexagon pair's merges
    * moved to the pair's new lower row (the old upper one). The mirror reverses the drift, so a residue m of
    * a pair whose lower row has rank ρ among the N rows of its axis becomes m + ρ − N + 1 (modulo the pattern
    * period). A mirror image has the same symbol; `StripSymmetrySpec` checks the keys agree.
    */
  def reverseLayers(w: Vector[Layer]): Vector[Layer] =
    val n        = w.size
    val (nu, nw) = rows(w)
    var ru       = 0
    var rw       = 0
    val rank     = w.map {
      case Layer.Tri(true, _, _)  => ru += 1; ru - 1
      case Layer.Tri(false, _, _) => rw += 1; rw - 1
      case Layer.Cubic            => -1
    }
    val out      = Array.tabulate(n) { j =>
      w(n - 1 - j) match
        case Layer.Cubic         => Layer.Cubic
        case Layer.Tri(ax, _, _) => Layer.Tri(ax, Set.empty, 1)
    }
    for i <- 0 until n do
      w(i) match
        case Layer.Tri(ax, hx, per) if hx.nonEmpty =>
          val shift = rank(i) - (if ax then nu else nw) + 1
          out(mod(n - 2 - i, n)) = Layer.Tri(ax, hx.map(m => mod(m + shift, per)), per)
        case _                                     => ()
    out.toVector

  /** The chains s · σ(s) · σ²(s) · … of a segment under the stack translation by the segment composed with an
    * in-plane isometry, each new segment the image of the previous one placed at the offsets reached so far
    * (the drift of the model supplies the half-unit translations); every chain of length at most maxLen, up
    * to the one that closes (the next segment being s again at its own offsets), or none if the isometry does
    * not map the segment's grid onto the next one's.
    */
  def chains(s: Vector[Layer], g: InPlane, maxLen: Int): Vector[Vector[Layer]] =
    val out   = Vector.newBuilder[Vector[Layer]]
    var w     = s
    var prev  = s
    var start = (0, 0)
    var go    = true
    while go && w.size + s.size <= maxLen do
      val target = rows(w)
      image(g, prev, start, target) match
        case Some(next) if next != s =>
          w = w ++ next
          out += w
          start = target
          prev = next
        case _                       => go = false
    out.result()

  /** A symmetry of a cyclic word of n layers acting on its junctions: j ↦ a·j + b (mod n), a = ±1. */
  final case class JunctionMap(a: Int, b: Int, n: Int):
    def apply(j: Int): Int = mod(a * j + b, n)

  /** Every stack symmetry of a cyclic word: the translations (a rotation r with g(w) rotated by r equal to w
    * moves layer j + r onto layer j, hence junction j ↦ j − r) and the reflections (g applied to the reversed
    * word, rotated by r, equal to w moves layer n − 1 − j − r onto layer j, hence the junction below a layer
    * onto the junction above its image: j ↦ n − r − j).
    */
  def wordSymmetries(w: Vector[Layer], group: Vector[InPlane]): Vector[JunctionMap] =
    val n   = w.size
    val rev = reverseLayers(w)
    val out = Vector.newBuilder[JunctionMap]
    for g <- group do
      for gw <- apply(g, w); r <- 0 until n if rotate(gw, r) == w do out += JunctionMap(1, -r, n)
      for gr <- apply(g, rev); r <- 0 until n if rotate(gr, r) == w do out += JunctionMap(-1, n - r, n)
    out.result()

  /** The orbit index of every junction under the given symmetries (a union–find over their images). */
  def junctionOrbits(n: Int, syms: Vector[JunctionMap]): Vector[Int] =
    val parent            = Array.tabulate(n)(identity)
    def find(a: Int): Int = if parent(a) == a then a else { parent(a) = find(parent(a)); parent(a) }
    for s <- syms; j <- 0 until n do
      val (x, y) = (find(j), find(s(j)))
      if x != y then parent(x) = y
    val ids               = collection.mutable.LinkedHashMap.empty[Int, Int]
    (0 until n).toVector.map(j => ids.getOrElseUpdate(find(j), ids.size))

  /** The species per junction orbit of a closed two-direction word, given the species at each junction
    * (junction j between layers j − 1 and j): None if a junction's species are unknown or if two orbits share
    * a species (the symbol would repeat a species across vertex orbits, so it is not Krötenheerdt); otherwise
    * the orbits' species sets, whose sizes sum to a lower bound of the vertex orbits. Not valid for words
    * with rows of one axis only (lifts), whose symmetries need not preserve the stack.
    */
  def orbitSpecies(
      w: Vector[Layer],
      group: Vector[InPlane],
      speciesAt: Int => Set[Int]
  ): Option[Vector[Set[Int]]] =
    val n      = w.size
    val orbits = junctionOrbits(n, wordSymmetries(w, group))
    val per    = (0 until n).groupBy(orbits).toVector.sortBy(_._1).map(_._2)
    val sets   = per.map(js => js.map(speciesAt).reduce(_ ++ _))
    if sets.exists(_.contains(-1)) then None
    else if sets.map(_.size).sum != sets.reduce(_ ++ _).size then None
    else Some(sets)

  /** True when the word has rows of both axes (a two-direction stacking, where the stack axis is canonical).
    */
  def twoDirection(w: Vector[Layer]): Boolean =
    val (nu, nw) = rows(w)
    nu > 0 && nw > 0

  /** A canonical form of an OPEN segment under the in-plane group (no rotation, no reversal): the least
    * label. Two segments with the same form generate the same chains up to symmetry, and so do all their
    * extensions, since the isometry acts on a prefix independently of what follows it.
    */
  def canonicalSegment(s: Vector[Layer], group: Vector[InPlane]): String =
    group.iterator.flatMap(g => apply(g, s)).map(StripStacking.showLayers).min

  /** A canonical form of a cyclic word under rotation, reversal and the in-plane group: the least label. */
  def canonicalLayers(w: Vector[Layer], group: Vector[InPlane]): String =
    val variants =
      for a <- Vector(w, reverseLayers(w)); g <- group; b <- apply(g, a).toVector; r <- b.indices
      yield rotate(b, r)
    variants.map(StripStacking.showLayers).min
