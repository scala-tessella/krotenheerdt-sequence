package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesCorona

import io.github.scala_tessella.research_core.Sigma0Assembly.unionOf
import Sigma0Orbits.{intertwines, orbitsOf}
import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf, Perm}

import java.nio.file.{Files, Path, Paths}

/** THE CONNECTIVITY GATE — the lookup side of the connectivity obstruction, split out so that a census driver
  * can skip theorem-certified tuples without rebuilding any of it.
  *
  * The theorem is `conn-BAD ⟹ σ₀-empty` with its pairwise reduction (a conn edge depends only on the two
  * folded parts), so a k-tuple's verdict is the connectivity of a k-vertex graph read off the k(k−1)/2
  * per-subgroup-pair tables. What the gate trusts operationally is not the theorem but THIS IMPLEMENTATION,
  * cross-validated at k = 4 against independent SAT refutation: 271,821 table-certified tuples, zero
  * disagreements. The census rows of the paper were produced with the gate off (`-Dcensus.gate=off`).
  *
  * CANONICALIZATION DRIFT IS THE CORRECTNESS RISK, so there is exactly one of everything here: one
  * [[canonicalSubs]], one [[buildTable]], one [[Gate.connOK]]. `ConnTableSweep` is a driver over this object
  * rather than a second copy of it — a gate that indexed subgroups differently from the builder would skip
  * live tuples silently, and no downstream count would look wrong.
  *
  * Tables live under `target/conn-tables/` (a cache, rebuilt on a clean checkout). Each carries the canonical
  * subgroup-order line of both species; the gate re-verifies it against the live lattice on load, so a
  * lattice-affecting change fails loudly instead of corrupting verdicts, and [[Gate.fingerprints]] names the
  * exact artifacts a run trusted.
  */
object ConnGate:

  private val tableDir = "target/conn-tables"

  /** `-Dcensus.gate=conn|off` (default `conn`). `conn` skips theorem-certified tuples; `off` reproduces the
    * pre-gate behaviour, which is what the census certificates of the paper were produced with.
    */
  def mode: String = sys.props.get("census.gate").map(_.trim.toLowerCase).getOrElse("conn")

  /** True when the gate is on. */
  def enabled: Boolean = mode == "conn"

  /** Run-independent ordering of a species' subgroup lattice (order, then canonical permutation string). THE
    * canonicalization: the builder, the loader and the gate all index through this one function.
    */
  def canonicalSubs(i: Int): Vector[Set[Perm]] =
    subgroupsOfSpecies(i).sortBy(sub => (sub.size, sub.toVector.map(_.mkString(",")).sorted.mkString(";")))

  /** The conn table of a species pair over canonical subgroup indices: bits(ai)(bi) = the 2-part union
    * carries a cross-part intertwiner.
    */
  def buildTable(i: Int, j: Int): Vector[Vector[Boolean]] =
    val (si, sj) = (symmetryOf(i), symmetryOf(j))
    val fbs      = canonicalSubs(j).map(hb => fold(sj, hb))
    canonicalSubs(i).map { ha =>
      val fa = fold(si, ha)
      fbs.map { fb =>
        val u      = unionOf(Vector(fa, fb))
        val orbits = orbitsOf(u)
        val partOf = orbits.map(o => u.orbit(o.head))
        orbits.indices.exists(a =>
          orbits.indices.exists(b => a < b && partOf(a) != partOf(b) && intertwines(u, orbits(a), orbits(b)))
        )
      }
    }

  def tablePath(i: Int, j: Int): Path = Paths.get(s"$tableDir/conn-pair-$i-$j.txt")

  private def writeTable(i: Int, j: Int, bits: Vector[Vector[Boolean]]): Unit =
    Files.createDirectories(Paths.get(tableDir)): Unit
    val sb = new StringBuilder
    sb ++= s"conn pair $i $j -- ${SpeciesCorona.label(i)} ~ ${SpeciesCorona.label(j)}\n"
    sb ++= s"orders ${canonicalSubs(i).map(_.size).mkString(" ")}\n"
    sb ++= s"orders ${canonicalSubs(j).map(_.size).mkString(" ")}\n"
    for row <- bits do sb ++= row.map(b => if b then '1' else '0').mkString + "\n"
    Files.write(tablePath(i, j), sb.toString.getBytes): Unit

  /** Load a persisted table, or build and persist it. The orders line is verified against the canonical
    * lattice — a stale file after a lattice-affecting change must fail loudly, not corrupt verdicts.
    */
  def loadOrBuildTable(i: Int, j: Int, say: String => Unit = _ => ()): Vector[Vector[Boolean]] =
    val p = tablePath(i, j)
    if Files.exists(p) then
      val lines   = Files.readAllLines(p).toArray(Array.empty[String]).toVector
      val ordersA = lines(1).stripPrefix("orders ").split(" ").map(_.toInt).toVector
      val ordersB = lines(2).stripPrefix("orders ").split(" ").map(_.toInt).toVector
      require(
        ordersA == canonicalSubs(i).map(_.size) && ordersB == canonicalSubs(j).map(_.size),
        s"stale conn table ${tablePath(i, j)}: subgroup lattice changed — delete and rebuild"
      )
      lines.drop(3).filter(_.nonEmpty).map(_.map(_ == '1').toVector)
    else
      val t0   = System.currentTimeMillis
      val bits = buildTable(i, j)
      writeTable(i, j, bits)
      say(s"  built table $i ~ $j (${bits.size} x ${bits.head.size}) in ${System.currentTimeMillis - t0} ms")
      bits

  /** SHA-256 of a table file, so a certificate names the exact artifact its verdicts trusted. */
  def fingerprint(p: Path): String =
    val md = java.security.MessageDigest.getInstance("SHA-256")
    md.digest(Files.readAllBytes(p)).map(b => f"$b%02x").mkString

  /** The gate of one k-set: the k(k−1)/2 tables loaded once, then a verdict per tuple in microseconds.
    *
    * `idx` is the tuple in CANONICAL subgroup indices, one per role, in the set's own species order — the
    * same convention the builder and `latticeSizes` use.
    */
  final class Gate private[ConnGate] (
      val sps: Vector[Int],
      private val tables: Map[(Int, Int), Vector[Vector[Boolean]]]
  ):
    val k: Int = sps.size

    /** Per role, the folded part sizes over the canonical lattice (free actions: chambers / |H|). */
    val latticeSizes: Vector[Vector[Int]] =
      sps.map(i => canonicalSubs(i).map(symmetryOf(i).cx.chambers.size / _.size))

    /** The union size of a tuple — arithmetic, no union built. */
    def unionSize(idx: Vector[Int]): Int = sps.indices.map(r => latticeSizes(r)(idx(r))).sum

    /** The trivial-fold union size: the ceiling of the residue band. */
    val trivialUnion: Int = sps.map(i => symmetryOf(i).cx.chambers.size).sum

    /** Connectivity of the k-vertex graph on table lookups. `false` = conn-BAD = theorem-certified σ₀-empty.
      */
    def connOK(idx: Vector[Int]): Boolean = connOK(idx.toArray)

    /** The same verdict without boxing the tuple — the walker reaches this one 2.16 billion times over the k =
      * 6 band, and a `Vector[Int]` per leaf is seven allocations that buy nothing.
      */
    def connOK(idx: Array[Int]): Boolean =
      var reach = 1
      var grew  = true
      while grew do
        grew = false
        for
          r <- 0 until k if (reach & (1 << r)) != 0
          s <- 0 until k if (reach & (1 << s)) == 0
        do
          val (a, b) = if r < s then (r, s) else (s, r)
          if tables((a, b))(idx(a))(idx(b)) then
            reach |= 1 << s
            grew = true
      reach == (1 << k) - 1

    /** The table artifacts this gate read, for the citation block of a certificate. */
    def fingerprints: Vector[(String, String)] =
      (for r <- 0 until k; s <- r + 1 until k yield
        val p = tablePath(sps(r), sps(s))
        p.getFileName.toString -> fingerprint(p)
      ).toVector

  /** THE GATED TUPLE WALK, shared by the scoped census and the band sweep.
    *
    * Visits the folding tuples of `sps` with `minChambers < union <= maxChambers` by bounded DFS — `minRest`
    * prunes whole subtrees rather than leaves — and calls `emit` on exactly those the beta1 gate leaves
    * alive. Nothing is accumulated here: the k = 6 band is 2.16 BILLION tuples across the corpus and 181 M on
    * one set, so a walker that built the product before filtering could not run at all.
    *
    * `idx` is REUSED between calls — an emitter that keeps it must copy. Returns the visited and conn-BAD
    * counts a certificate has to disclose.
    */
  def gatedWalk(sps: Vector[Int], minChambers: Int, maxChambers: Int)(
      emit: (Array[Int], Int) => Unit
  ): (Long, Long) =
    val gate                          = of(sps)
    val sizes                         = gate.latticeSizes
    val k                             = sps.size
    val minRest                       = Array.fill(k + 1)(0)
    for r <- k - 1 to 0 by -1 do minRest(r) = minRest(r + 1) + sizes(r).min
    val idx                           = Array.fill(k)(0)
    var inBand                        = 0L
    var connbad                       = 0L
    def walk(r: Int, used: Int): Unit =
      if r == k then
        if used > minChambers then
          inBand += 1
          if gate.connOK(idx) then emit(idx, used) else connbad += 1
      else
        for ix <- sizes(r).indices do
          val u = used + sizes(r)(ix)
          if u + minRest(r + 1) <= maxChambers then
            idx(r) = ix
            walk(r + 1, u)
        idx(r) = 0
    walk(0, 0)
    (inBand, connbad)

  /** The survivors as canonical subgroup indices with their union size, in union order (most-folded first,
    * the order `kEntries` sorts by). For the SCOPED census, whose slivers are small — the band sweep uses
    * [[gatedPacked]], which holds the same information in a fraction of the memory.
    */
  def gatedTuples(
      sps: Vector[Int],
      minChambers: Int,
      maxChambers: Int
  ): (Vector[(Vector[Int], Int)], Long, Long) =
    val out               = Vector.newBuilder[(Vector[Int], Int)]
    val (inBand, connbad) = gatedWalk(sps, minChambers, maxChambers)((idx, u) => out += ((idx.toVector, u)))
    (out.result().sortBy(_._2), inBand, connbad)

  /** Whether a k-set's survivors can be packed: one byte per role index, and the union above them. */
  def packable(sps: Vector[Int]): Boolean =
    sps.size <= 6 && sps.forall(i => canonicalSubs(i).size <= 255)

  def packedUnion(word: Long, k: Int): Int = (word >>> (8 * k)).toInt

  def packedIndex(word: Long, k: Int, r: Int): Int = ((word >>> (8 * (k - 1 - r))) & 0xffL).toInt

  /** The survivors PACKED, one `Long` each: the union in the high bits, then one byte per canonical subgroup
    * index. A boxed `Vector[(Vector[Int], Int)]` costs some 150 bytes a tuple, and the k = 6 band's biggest
    * set has millions of survivors — several GB per worker, with the biggest sets scheduled first. Packed
    * they are 8 bytes each, and because the union sits in the high bits, sorting the words IS sorting by
    * union, so the sweep order is unchanged.
    */
  def gatedPacked(sps: Vector[Int], minChambers: Int, maxChambers: Int): (Array[Long], Long, Long) =
    require(packable(sps), s"unpackable k-set: ${sps.mkString(":")}")
    val k                 = sps.size
    var buf               = new Array[Long](1024)
    var n                 = 0
    val (inBand, connbad) = gatedWalk(sps, minChambers, maxChambers) { (idx, u) =>
      if n == buf.length then buf = java.util.Arrays.copyOf(buf, buf.length * 2)
      var w = u.toLong
      for r <- 0 until k do w = (w << 8) | (idx(r).toLong & 0xffL)
      buf(n) = w
      n += 1
    }
    val out               = java.util.Arrays.copyOf(buf, n)
    java.util.Arrays.sort(out) // the union is in the high bits: this is the union order
    (out, inBand, connbad)

  /** Build the gate of a k-set, loading (or building and persisting) every pair table it needs. */
  def of(sps: Vector[Int], say: String => Unit = _ => ()): Gate =
    val k = sps.size
    require(k >= 2, s"a gate needs at least two roles: ${sps.mkString(":")}")
    require(sps == sps.sorted, s"species must be in canonical (sorted) order: ${sps.mkString(":")}")
    Gate(
      sps,
      (for r <- 0 until k; s <- r + 1 until k
      yield (r, s) -> loadOrBuildTable(sps(r), sps(s), say)).toMap
    )
