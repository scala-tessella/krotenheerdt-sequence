package io.github.scala_tessella.krotenheerdt_sequence

/** The hexagon world: the Krötenheerdt classification of the honeycombs with truncated tetrahedra. By the
  * paper's hexagon-world theorem such a honeycomb is a stack of quarter-cubic slabs S and kagome prism layers
  * K glued along kagome planes. At a kagome plane the S side carries one tetrahedron at every vertex (F2),
  * and each vertex touches one kagome triangle of each orientation, so the slab's tetrahedra stand on one
  * whole class of triangles: S_b, b ∈ {0, 1} the class of its lower face. Its downward tetrahedra share their
  * apices with the upward ones, point-reflected, so its upper face carries them on the other class 1 − b. A K
  * layer keeps the kagome plane and its classes.
  *
  * Levels with vertices: the junction planes and the apex level inside every S slab. Species (F1, F2):
  *
  * {{{
  * K | K                      21 {p3:4 p6:4}#3            (the kagome lift)
  * S | K, K | S               22 {tet:1 truncTet:3 p3:2 p6:2}#1
  * S_a | S_b, 1 − a == b      23 {tet:2 truncTet:6}#1     (two tetrahedra sharing a face)
  * S_a | S_b, 1 − a != b      24 {tet:2 truncTet:6}#2     (two tetrahedra sharing a vertex only)
  * the apex level of S        24 {tet:2 truncTet:6}#2
  * }}}
  *
  * Congruence: shifts, the class swap (a 60° turn of the kagome plane), reversal (which reads every slab
  * upside down, S_b ↦ S_{1−b}). The quarter-cubic honeycomb, the word S_0 (every junction and apex 24, one
  * orbit by a symmetry that does not preserve the layering), is the degenerate 1-uniform class, counted by
  * the 28; every other word admits only its layering, so isometries induce word symmetries, and the vertex
  * orbits are the level orbits. Four species: no Krötenheerdt class for k ≥ 5, and the quasi-period bound m ≤
  * 2k of the slab world applies verbatim.
  */
object HexagonNecklaces:

  enum Layer:
    case K, S0, S1
    def swap: Layer = this match
      case K  => K
      case S0 => S1
      case S1 => S0

  import Layer.*

  def junction(lo: Layer, hi: Layer): Int =
    (lo, hi) match
      case (K, K)                           => 21
      case (K, _) | (_, K)                  => 22
      case (a, b) if (a == S0) == (b == S1) => 23 // 1 − a == b: the classes meet equal
      case _                                => 24

  /** Levels in order: the junction below layer i (level 2i) and, for S, its apex level (2i + 1). */
  def levels(w: Vector[Layer]): Vector[(Int, Int)] =
    val n = w.size
    w.indices.toVector.flatMap { i =>
      val j = (2 * i, junction(w((i + n - 1) % n), w(i)))
      if w(i) == K then Vector(j) else Vector(j, (2 * i + 1, 24))
    }

  /** The level orbits under the word's symmetries, each with its species set. */
  def orbits(w: Vector[Layer]): Vector[Set[Int]] =
    val n                           = w.size
    val p                           = 2 * n
    val parent                      = Array.tabulate(p)(identity)
    def find(x: Int): Int           = if parent(x) == x then x else { parent(x) = find(parent(x)); parent(x) }
    def union(a: Int, b: Int): Unit = parent(find(a)) = find(b)
    def mod(x: Int, m: Int): Int    = ((x % m) + m) % m
    for
      m <- 1 until n
      e <- List(false, true)
      if w.indices.forall(i => w(mod(i + m, n)) == (if e then w(i).swap else w(i)))
    do (0 until p).foreach(q => union(q, mod(q + 2 * m, p)))
    for
      t <- 0 until n
      e <- List(false, true)
      if w.indices.forall(j => w(mod(t - j, n)) == (if e then w(j) else w(j).swap))
    do (0 until p).foreach(q => union(q, mod(2 * t + 2 - q, p)))
    levels(w).groupMap((q, _) => find(q))(_._2).values.toVector.map(_.toSet)

  def minimalPeriod(w: Vector[Layer]): Vector[Layer] =
    val n = w.size
    w.take((1 to n).find(d => n % d == 0 && w.indices.forall(i => w(i) == w(i % d))).get)

  /** Canonical representative: the lexicographic maximum over shifts, the class swap and reversal. */
  def canonical(w0: Vector[Layer]): Vector[Layer] =
    import scala.math.Ordering.Implicits.seqOrdering
    given Ordering[Layer] = Ordering.by(_.ordinal)
    val w                 = minimalPeriod(w0)
    (for
      rev <- Vector(false, true)
      e   <- Vector(false, true)
      r   <- w.indices.toVector
      base = if rev then w.reverse.map(_.swap) else w
      b2   = if e then base.map(_.swap) else base
    yield b2.drop(r) ++ b2.take(r)).max

  final case class NecklaceClass(word: Vector[Layer], species: Vector[Int]):
    def k: Int       = species.size
    def show: String = word.map { case K => "K"; case S0 => "S0"; case S1 => "S1" }.mkString(" ")

  private def words(n: Int): Iterator[Vector[Layer]] =
    Iterator.range(0, math.pow(3, n).toInt).map(x =>
      Vector.tabulate(n)(i => Layer.fromOrdinal((x / math.pow(3, i).toInt) % 3))
    )

  /** The Krötenheerdt necklaces with at least one slab and at least two orbits, over quasi-periods up to
    * `maxLength` (complete for 2·4 = 8 by the quasi-period bound).
    */
  def enumerate(maxLength: Int): Vector[NecklaceClass] =
    val found = collection.mutable.LinkedHashMap.empty[Vector[Layer], NecklaceClass]
    for
      n     <- 1 to maxLength
      w     <- words(n)
      if w.exists(_ != K)
      twist <- List(false, true)
    do
      val key = canonical(if twist then w ++ w.map(_.swap) else w)
      if !found.contains(key) then
        val os = orbits(key)
        if os.forall(_.size == 1) then
          val sp = os.map(_.head)
          if sp.distinct.size == sp.size then found(key) = NecklaceClass(key, sp.sorted)
    found.values.toVector.sortBy(c => (c.k, c.word.size, c.show))

  lazy val classes: Vector[NecklaceClass] = enumerate(8)
