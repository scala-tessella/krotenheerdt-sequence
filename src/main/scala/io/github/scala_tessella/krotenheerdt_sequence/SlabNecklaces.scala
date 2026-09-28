package io.github.scala_tessella.krotenheerdt_sequence

/** The slab world: the complete Krötenheerdt classification of the slab alphabet {tet, oct, P3}. The
  * structure theorem (proved in the paper, its finite layers certified here and in the spec): every
  * face-to-face unit-edge honeycomb with cells among {tet, oct, P3} is layered by complete planes in exactly
  * one of two ways.
  *
  *   - VERTICAL WORLD (a tet or oct is present, or all prisms are parallel): horizontal triangular-lattice
  *     planes; the slab between consecutive planes is EITHER an octet slab (up-tets + octahedra + down-tets,
  *     the octet slab of the close packings — the lattice position steps to one of the two other positions)
  *     OR a prism slab (unit-height P3 columns on all triangles — the position stays). The mixed-slab option
  *     dies by Niven: above an in-plane edge the based pair must close a straight angle, and τ + 90 ≠ 180,
  *     (180−τ) + 90 ≠ 180, so the two based cells are {tet, oct} or {P3, P3} and connectivity makes the
  *     choice slab-wide. Encoding: one step per slab from {+1, −1} (octet, the two shift senses of the
  *     3-cycle A→B→C) or {0} (prism). Position relabelings act on steps as: 3-cycles fix them, transpositions
  *     negate them; a horizontal mirror reverses slab order and negates. Congruence is therefore equality of
  *     step sequences up to shift, negation and reversal, and every step sequence is realizable by stacking
  *     the slabs.
  *   - HORIZONTAL WORLD (all-P3 with a perpendicular-axis contact somewhere): square-lattice planes; every
  *     layer is a lying strip layer of thickness √3/2 (triangular-strip cross-section × unit segmentation)
  *     whose axis is one of two perpendicular horizontal directions, and the interface grid forces each layer
  *     with no residual freedom. Encoding: one letter per junction, s (same axis continues — the two layers
  *     merge into lying triangular-prismatic order) or g (the axis switches — the gyrobifastigium gluing).
  *     Congruence = junction words up to shift and reversal; every word is realizable. The all-s word IS the
  *     triangular prismatic honeycomb = the all-0 vertical word: the single overlap of the two worlds,
  *     counted once.
  *
  * All vertices lie on junction planes and in-plane translations are transitive on each junction's vertices,
  * so vertex orbits = junction orbits under the symmetry group of the word (for the degenerate all-c and
  * all-0 words the honeycomb is 1-uniform and the count is 1 either way; every other word admits only the
  * horizontal layering, so isometries induce word symmetries). The junction species — the species table
  * restricted to this alphabet has EXACTLY five, asserted in the spec:
  *
  * {{{
  * c = {tet:8 oct:6} #2   (cuboctahedral, fcc)          octet·octet junction, equal steps
  * h = {tet:8 oct:6} #1   (orthobicupola, hcp)          octet·octet junction, opposite steps
  * e = {tet:4 oct:3 p3:6} (the elongated star)          octet·prism junction, either order, gyration-blind
  * p = {p3:12} #2         (parallel prisms)             prism·prism junction = horizontal s-junction
  * x = {p3:12} #1         (mixed-axis, gyrobifastigium) horizontal g-junction
  * }}}
  *
  * Krötenheerdt k-uniform = exactly k junction orbits carrying k pairwise distinct species. TERMINATION: a
  * stabilizer without a translation contains at most one reflection (two reflections compose to a
  * translation) and so has infinitely many orbits; hence k orbits force a minimal quasi-translation (shift by
  * m with sign ε), all reflections in the stabilizer descend to a SINGLE reflection of ℤ/m, and the orbit
  * count is m (no reflection) or (m + fixed)/2 ≥ m/2. So the quasi-period is at most 2k, and the species
  * pigeonhole (≤ 4 species live in the vertical world, ≤ 2 in the horizontal, and x forces the horizontal
  * world) finishes: the classification is the finite enumeration below, and the Krötenheerdt sequence of the
  * slab alphabet vanishes from k = 5 on.
  */
object SlabNecklaces:

  /** The five vertex species of the slab alphabet (the species table restricted to cells ⊆ {tet, oct, P3}).
    */
  enum Species(val label: String):
    case C    extends Species("c") // {tet:8 oct:6} #2 — cuboctahedral star (fcc)
    case H    extends Species("h") // {tet:8 oct:6} #1 — orthobicupola star (hcp)
    case E    extends Species("e") // {tet:4 oct:3 p3:6} — the elongated star
    case Ppar extends Species("p") // {p3:12} #2 — parallel prism star
    case Pmix extends Species("x") // {p3:12} #1 — mixed-axis (gyrobifastigium) star

  import Species.*

  /** Junction species between two consecutive vertical slabs with the given steps. */
  def junctionOf(prev: Int, next: Int): Species =
    if prev == 0 && next == 0 then Ppar
    else if prev == 0 || next == 0 then E
    else if prev == next then C
    else H

  /** One full period of junction species of a full-period vertical step word. */
  def junctions(word: Vector[Int]): Vector[Species] =
    word.indices.toVector.map(i => junctionOf(word((i + word.size - 1) % word.size), word(i)))

  /** The full plain period of a quasi-periodic word: twist −1 doubles with negated steps. */
  def fullWord(steps: Vector[Int], twist: Int): Vector[Int] =
    if twist == 1 then steps else steps ++ steps.map(-_)

  /** Reduce a periodic word to its minimal plain period. */
  def minimalPeriod(w: Vector[Int]): Vector[Int] =
    val n = w.size
    val d = (1 to n).find(d => n % d == 0 && w.indices.forall(i => w(i) == w(i % d))).get
    w.take(d)

  /** Minimal quasi-period: the smallest m ≥ 1 with w(i+m) = ε·w(i) for all i, some ε ∈ {±1}. */
  def minimalQuasiPeriod(w: Vector[Int]): Int =
    val n = w.size
    (1 to n)
      .find(m => List(1, -1).exists(e => w.indices.forall(i => w((i + m) % n) == e * w(i))))
      .get

  /** Canonical representative of the congruence class: lexicographic MAX over shifts, negation, reversal,
    * after minimal-period reduction. (For the horizontal world negation is vacuous — letters are 0/1 and the
    * swap of the two axis directions fixes every junction letter.)
    */
  def canonical(w0: Vector[Int]): Vector[Int] =
    import scala.math.Ordering.Implicits.seqOrdering
    val w        = minimalPeriod(w0)
    val n        = w.size
    val variants =
      for
        neg <- Vector(false, true)
        rev <- Vector(false, true)
        r   <- (0 until n).toVector
        base = if rev then w.reverse else w
        spun = base.drop(r) ++ base.take(r)
      yield if neg then spun.map(-_) else spun
    variants.max

  /** Junction species of a horizontal word (whose letters ARE the junctions: 0 = s, 1 = g). */
  def horizontalJunctions(word: Vector[Int]): Vector[Species] =
    word.map(l => if l == 0 then Ppar else Pmix)

  /** Junction orbits of a full-period word under its stabilizer, computed on ℤ/P, P the full period. Vertical
    * words index SLABS: shifts-with-sign (m, ε) act on junction planes as i ↦ i+m; reflections w(t−j) =
    * δ·w(j) (slab j ↦ t−j; δ = − for a plain mirror, δ = + for mirror + transposition relabel) act on planes
    * as i ↦ t+1−i. Horizontal words index the JUNCTIONS themselves: signs are vacuous (the axis swap fixes
    * every letter) and a reflection w(t−i) = w(i) acts as i ↦ t−i.
    */
  final case class Orbits(orbitOf: Vector[Int], census: Vector[(Species, Int)]):
    def count: Int               = census.size
    def speciesSet: Set[Species] = census.map(_._1).toSet
    def isKroetenheerdt: Boolean = census.size == speciesSet.size

  def orbits(w: Vector[Int], vertical: Boolean): Orbits =
    val n                           = w.size
    val parent                      = Array.tabulate(n)(identity)
    def find(x: Int): Int           =
      var r = x
      while parent(r) != r do r = parent(r)
      var c = x
      while parent(c) != c do { val nx = parent(c); parent(c) = r; c = nx }
      r
    def union(a: Int, b: Int): Unit =
      val (ra, rb) = (find(a), find(b))
      if ra != rb then parent(ra) = rb
    def mod(x: Int): Int            = ((x % n) + n) % n
    val signs                       = if vertical then List(1, -1) else List(1)
    val off                         = if vertical then 1 else 0
    for
      m <- 1 until n
      e <- signs
      if w.indices.forall(i => w(mod(i + m)) == e * w(i))
    do w.indices.foreach(i => union(i, mod(i + m)))
    for
      t <- 0 until n
      d <- signs
      if w.indices.forall(j => w(mod(t - j)) == d * w(j))
    do w.indices.foreach(i => union(i, mod(t + off - i)))
    val js                          = if vertical then junctions(w) else horizontalJunctions(w)
    val roots                       = w.indices.toVector.map(find)
    val byRoot                      = w.indices.groupBy(find).toVector
    val census                      = byRoot.map { (_, is) =>
      val sp = is.map(js).distinct
      require(sp.size == 1, s"orbit with mixed species in ${w.mkString(",")} — symmetry bug")
      (sp.head, is.size)
    }
    Orbits(roots, census.sortBy(o => (o._1.ordinal, o._2)))

  /** A Krötenheerdt class of the slab alphabet: a canonical word of one of the two worlds. */
  final case class NecklaceClass(
      vertical: Boolean,
      word: Vector[Int],
      orbitData: Orbits
  ):
    def k: Int                        = orbitData.count
    def junctionWord: Vector[Species] =
      if vertical then junctions(word) else horizontalJunctions(word)
    def showWord: String              =
      if vertical then word.map { case 1 => '+'; case -1 => '-'; case _ => '0' }.mkString
      else word.map(l => if l == 0 then 's' else 'g').mkString
    def showJunctions: String         = junctionWord.map(_.label).mkString

    /** Vertical only: the lattice-position period (the polytype period in layers). */
    def positionPeriod: Option[Int] =
      Option.when(vertical)(if word.sum % 3 == 0 then word.size else 3 * word.size)
    def name: Option[String]        = SlabNecklaces.nameOf(this)
    def show: String                =
      val world = if vertical then "vertical  " else "horizontal"
      val nm    = name.map(n => s"  = $n").getOrElse("")
      val pp    = positionPeriod.map(p => s"  positions/$p").getOrElse("")
      f"$world  [$showWord%-12s]  junctions $showJunctions%-12s  orbits ${orbitData.census
          .map((s, m) => s"${s.label}:$m")
          .mkString(" ")}%-14s$pp$nm"

  /** Krötenheerdt classes vanish beyond k = 4 (species pigeonhole); enumeration cap 2·maxK is the proved
    * quasi-period bound.
    */
  val maxK = 5

  private def words(alphabet: List[Int], p: Int): Iterator[Vector[Int]] =
    Iterator
      .iterate(Option(Vector.fill(p)(alphabet.head))) {
        case Some(w) =>
          def inc(i: Int, acc: Vector[Int]): Option[Vector[Int]] =
            if i < 0 then None
            else
              val next = alphabet.indexOf(acc(i)) + 1
              if next < alphabet.size then Some(acc.updated(i, alphabet(next)))
              else inc(i - 1, acc.updated(i, alphabet.head))
          inc(p - 1, w)
        case None    => None
      }
      .takeWhile(_.isDefined)
      .map(_.get)

  /** Krötenheerdt classes (orbit count ≤ maxK, distinct species) found over the given quasi-periods, one per
    * canonical word. The p ≤ 2·maxK range is COMPLETE by the quasi-period bound; larger ranges serve as
    * over-sweep teeth in the spec.
    */
  def enumerate(verticalWorld: Boolean, quasiPeriods: Range): Vector[NecklaceClass] =
    val alphabet = if verticalWorld then List(1, -1, 0) else List(0, 1)
    val twists   = if verticalWorld then List(1, -1) else List(1)
    val found    = collection.mutable.LinkedHashMap.empty[Vector[Int], NecklaceClass]
    for
      p     <- quasiPeriods
      twist <- twists
      steps <- words(alphabet, p)
    do
      val w   = minimalPeriod(fullWord(steps, twist))
      val key = canonical(w)
      if !found.contains(key) then
        val os = orbits(key, verticalWorld)
        if os.count <= maxK && os.isKroetenheerdt then
          found(key) = NecklaceClass(verticalWorld, key, os)
    found.values.toVector.sortBy(c => (c.k, c.word.size, c.showWord))

  /** The Krötenheerdt classes of the vertical world (octet/prism stackings; includes the pure-octet Barlow
    * stackings and the all-prism triangular prismatic honeycomb).
    */
  lazy val verticalClasses: Vector[NecklaceClass] = enumerate(verticalWorld = true, 1 to 2 * maxK)

  /** The Krötenheerdt classes of the horizontal world, INCLUDING the all-s word (= vertical all-0). */
  lazy val horizontalClassesAll: Vector[NecklaceClass] = enumerate(verticalWorld = false, 1 to 2 * maxK)

  /** The complete Krötenheerdt classification of the slab alphabet: both worlds, the overlap (all-s = all-0
    * = triangular prismatic) counted once, in the vertical world.
    */
  lazy val classes: Vector[NecklaceClass] =
    (verticalClasses ++ horizontalClassesAll.filterNot(_.word.forall(_ == 0)))
      .sortBy(c => (c.k, !c.vertical, c.word.size, c.showWord))

  /** The Krötenheerdt sequence of the slab alphabet, k = 1..maxK (zero from k = 5 on, and provably zero for
    * every k > maxK by the quasi-period bound + species pigeonhole).
    */
  lazy val sequence: Vector[Int] = (1 to maxK).toVector.map(k => classes.count(_.k == k))

  /** Classical identifications: the six 1-uniform classes are the honeycombs of Grünbaum's 28 with cells in
    * the slab alphabet; the four pure-octet 2-uniform classes are the Barlow stackings, named by polytype.
    */
  def nameOf(c: NecklaceClass): Option[String] =
    val barlow = Map(
      (1, 1) -> "4H stacking (double-hcp, word hc)",
      (1, 2) -> "6H stacking (word hcc)",
      (2, 1) -> "9R stacking (samarium-type, word hhc)",
      (2, 2) -> "12R stacking (word hhcc)"
    )
    if c.vertical then
      c.word match
        case Vector(1)                         => Some("alternated cubic (fcc)")
        case Vector(0)                         => Some("triangular prismatic (= horizontal all-s)")
        case Vector(1, -1)                     => Some("gyrated alternated cubic (hcp)")
        case Vector(1, 0)                      => Some("elongated alternated cubic")
        case Vector(1, 0, -1, 0)               => Some("gyroelongated alternated cubic")
        case w if w.forall(_ != 0) && c.k == 2 =>
          val qjs = junctions(w).take(minimalQuasiPeriod(w))
          barlow.get((qjs.count(_ == H), qjs.count(_ == C)))
        case _                                 => None
    else
      c.word match
        case Vector(1) => Some("gyrated triangular prismatic")
        case Vector(0) => Some("triangular prismatic (counted in the vertical world)")
        case _         => None
