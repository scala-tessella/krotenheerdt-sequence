package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona

import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}
import io.github.scala_tessella.research_core.SymbolCatalog.*
import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionOf}
import SymbolRealizationFilter.{realizeK, Verdict}

/** THE k-GENERIC SCOPED CENSUS: per fair k-set, the folding-TUPLE lattice swept over every tuple with union ≤
  * `scope` chambers (uncapped in scope, `kEntries`), minimal symbols deduped by canonical key with full tuple
  * provenance, and EVERY census symbol pushed through the standalone k-role realization filter (`realizeK`,
  * escalating harvest). The cert records per k-set which tuples were covered and how many lie beyond scope —
  * the tail beyond it is swept by `CensusTails` and `CensusBands`.
  *
  * Args: k, scope, then optional explicit k-sets as "i:j:..." (species indices) — without them the fair
  * k-sets are derived (`KSetShell.fairKSets(k)`). Parallelism across k-sets via -Dcensus.par=N (shared caches
  * pre-warmed sequentially). Output: `target/census/symbols-k<k>.txt` (explicit runs append -partial-<n>).
  * Run: sbt -Dcensus.par=8 "Test/runMain io.github.scala_tessella.krotenheerdt_sequence.CensusScoped 4 96" —
  * redirect to a plain log file, never pipe to tail.
  */
object CensusScoped:

  final case class KSetResult(
      sps: Vector[Int],
      stats: Vector[AssemblyStatK],
      skippedCount: Long,
      inScope: Long,
      connbad: Long,
      entries: Vector[(KEntry, Verdict)]
  )

  /** All folding-tuple union sizes of a k-set (chamber sums over the full subgroup-lattice product). */
  def unionSizes(sps: Vector[Int]): Vector[Int] =
    sps.foldLeft(Vector(0)) { (acc, i) =>
      val sym = symmetryOf(i)
      for
        partial <- acc
        sub     <- subgroupsOfSpecies(i)
      yield partial + sym.cx.chambers.size / sub.size
    }

  /** The scoped census of ONE k-set: the in-scope folding-tuple sweep (`kEntries`, uncapped in scope) plus
    * every minimal symbol through the standalone realization filter. Messages carry no timestamp — the caller
    * wraps `say`.
    */
  /** How many folding tuples lie in scope, by a bounded-sum DP — `unionSizes` answers this by materialising
    * the lattice, which a k = 6 set of a quarter of a billion tuples cannot survive. Used to order a run's
    * sets by cost.
    */
  def inScopeCount(sps: Vector[Int], scope: Int): Long =
    val parts = sps.map(i => ConnGate.canonicalSubs(i).map(symmetryOf(i).cx.chambers.size / _.size))
    val cnt   = Array.ofDim[Long](scope + 1)
    cnt(0) = 1L
    for sizes <- parts do
      val next = Array.ofDim[Long](scope + 1)
      for s <- 0 to scope if cnt(s) > 0; v <- sizes if s + v <= scope do next(s + v) += cnt(s)
      Array.copy(next, 0, cnt, 0, scope + 1)
    cnt.sum

  /** The full folding-tuple lattice size, as a product — `unionSizes` materialises it, which a k = 6 set
    * cannot survive.
    */
  def latticeSize(sps: Vector[Int]): Long =
    sps.map(i => subgroupsOfSpecies(i).size.toLong).product

  /** `-Dcensus.gate=conn` (default) routes the sweep through [[gatedEntries]]; `off` keeps `kEntries`, which
    * is what every banked census cert was produced with.
    */
  def gated: Boolean = ConnGate.enabled

  /** THE GATED, STREAMING TWIN of `kEntries` — same result, reached without materialising the lattice.
    *
    * `kEntries` builds the whole folding-tuple product before filtering it by union size. At k = 4 that is
    * 4.3 M tuples and merely wasteful; at k = 6 the largest single set has a lattice of 249 M and the census
    * cannot be run at all. Here the product is walked by bounded DFS (`minRest` prunes subtrees, not leaves)
    * and each in-scope tuple is put to the β1 gate BEFORE any union is built, so the assembly only ever sees
    * the conn-OK sliver: measured at scope 128, 3,170,868 of 471,964,922 tuples at k = 6 (0.67%), and
    * 1,039,354 of 52,295,282 at k = 5 (1.99%).
    *
    * Skipping is sound because conn-BAD ⟹ σ₀-empty: a skipped tuple could carry no symbol, so no symbol can
    * be lost. That is a THEOREM the census now leans on, and the certificate says so once, explicitly, rather
    * than the count quietly changing — see `certText`.
    *
    * Stats are emitted for the assembled tuples only; `connbad` is returned separately and counted, since
    * emitting a row per skipped tuple would be hundreds of millions of rows.
    */
  def gatedEntries(
      sps: Vector[Int],
      maxChambers: Int,
      sigma0Cap: Int,
      log: String => Unit
  ): (Vector[KEntry], Vector[AssemblyStatK], Long, Long) =
    require(sps.distinct.size == sps.size, "Krötenheerdt k-sets carry pairwise distinct species")
    val subsOf                        = sps.map(ConnGate.canonicalSubs)
    val foldsOf                       = sps.indices.toVector.map(r => subsOf(r).map(fold(symmetryOf(sps(r)), _)))
    val out                           = collection.mutable.LinkedHashMap.empty[Vector[Int], KEntry]
    val stats                         = Vector.newBuilder[AssemblyStatK]
    // the walk, the gate and the union order live in ConnGate.gatedTuples — the band sweep uses the same one
    val (survivors, inScope, connbad) = ConnGate.gatedTuples(sps, minChambers = -1, maxChambers)
    for ((tuple, total), pi) <- survivors.zipWithIndex do
      val subs         = sps.indices.toVector.map(r => subsOf(r)(tuple(r)))
      val fs           = sps.indices.toVector.map(r => foldsOf(r)(tuple(r)))
      val u            = unionOf(fs)
      val (sols, capd) = enumerateSigma0(u, sigma0Cap, log)
      var fresh        = 0
      for s0 <- sols do
        val sym = symOf(u, sps, s0)
        if isMinimal(sym) then
          val key = canonicalKey(sym)
          out.get(key) match
            case None    => out(key) = KEntry(sps, sym, Vector(subs)); fresh += 1
            case Some(e) => if !e.folds.contains(subs) then out(key) = e.copy(folds = e.folds :+ subs)
      stats += AssemblyStatK(subs.map(_.size), total, false, sols.size, fresh, capd)
    (out.values.toVector, stats.result(), inScope, connbad)

  def runKSet(sps: Vector[Int], scope: Int, say: String => Unit): KSetResult =
    val tag                                = sps.map(SpeciesCorona.label).mkString(" ~ ")
    val flags                              = MonoShell.Flags()
    say(s"== start $tag ==")
    val (entries, stats, inScope, connbad) =
      if gated then gatedEntries(sps, scope, sigma0Cap = 100000, log = m => say(s"[$tag]$m"))
      else
        val (e, st) = kEntries(sps, maxChambers = scope, sigma0Cap = 100000, log = m => say(s"[$tag]$m"))
        (e, st, st.size.toLong, 0L)
    // beyond scope by DP, never by materialising the lattice: a k = 6 set reaches 249 M tuples
    val skipped                            = latticeSize(sps) - inScope
    if connbad > 0 then
      say(s"[$tag] gate: $inScope in scope, $connbad theorem-skipped, ${inScope - connbad} assembled")
    say(s"== $tag: ${entries.size} minimal symbols in scope, realizing each ==")
    val verdicts                           = entries.map { e =>
      val v = realizeK(e, flags, log = m => say(s"[$tag]$m"))
      say(
        s"[$tag] ${e.sym.size}-chamber symbol: " +
          (if v.realized then s"REALIZED (assembly ${v.assembly + 1})"
           else s"NOT REALIZED (candidates ${v.candidates}, capped ${v.capped})")
      )
      (e, v)
    }
    say(s"== done $tag: ${verdicts.count(_._2.realized)}/${entries.size} realized ==")
    KSetResult(sps, stats, skipped, inScope, connbad, verdicts)

  /** True iff the σ₀ sweep hit the cap on some in-scope tuple (census possibly incomplete in scope). */
  def sweepCapped(r: KSetResult): Boolean = r.stats.exists(_.capped)

  def certOf(dir: java.nio.file.Path, sps: Vector[Int]): java.nio.file.Path =
    dir.resolve(s"census-k${sps.size}-${sps.mkString("-")}.txt")

  /** The completion marker a per-set cert must carry for the driver to skip the set on resume: the sweep
    * uncapped at this scope AND every census symbol realized (a realization failure means a missing harvest
    * rung — the set re-runs after the rung is added).
    */
  def completeMarker(scope: Int): String = s"CENSUS COMPLETE in scope $scope (uncapped, all realized)"

  /** The per-set cert: the stats row, then per symbol the folds provenance, the realization verdict and the
    * CANONICAL KEY (one parseable `key` line each — the identification reads these instead of paying the
    * census again).
    */
  def kSetCertText(scope: Int, r: KSetResult): String =
    val k        = r.sps.size
    val realized = r.entries.count(_._2.realized)
    val sb       = new StringBuilder
    sb ++= s"k = $k scoped census -- ${r.sps.map(SpeciesCorona.label).mkString(" ~ ")}\n"
    sb ++= s"species ${r.sps.mkString(":")}  scope $scope\n"
    sb ++= "every folding tuple with union <= scope chambers, uncapped in scope; minimal symbols deduped\n"
    sb ++= "by canonical key; every symbol through realizeK (escalating harvest); key lines are the\n"
    sb ++= "canonical keys, comma-separated.\n\n"
    sb ++= f"asm ${r.stats.size}%7d skip ${r.skippedCount}%7d connbad ${r.connbad}%9d " +
      f"sym ${r.entries.size}%3d real $realized%3d" + (if sweepCapped(r) then "  CAPPED\n" else "\n")
    if r.connbad > 0 then
      sb ++= f"\nOF THE ${r.inScope}%d IN-SCOPE FOLDING TUPLES, ${r.connbad}%d WERE NOT ASSEMBLED: they are\n"
      sb ++= "conn-BAD, and the beta1 obstruction (conn-BAD => sigma0-empty, with its pairwise reduction)\n"
      sb ++= "certifies that such a tuple carries no symbol. The completeness claimed below is therefore\n"
      sb ++=
        "conditional on that theorem. Its implementation is ConnGate, cross-validated at k = 4 against\n"
      sb ++= "independent SAT refutation (271,821 table-certified tuples, zero disagreements). The tables\n"
      sb ++= "read, by content:\n"
      for (name, hash) <- ConnGate.of(r.sps).fingerprints do sb ++= s"  $name  $hash\n"

    for (e, v) <- r.entries do
      sb ++= s"symbol ${e.sym.size}ch at ${e.folds.map(_.map(_.size).mkString("x")).mkString(",")}: " +
        (if v.realized then s"REALIZED assembly ${v.assembly + 1}"
         else s"NOT REALIZED capped=${v.capped}") + "\n"
      sb ++= s"  key ${canonicalKey(e.sym).mkString(",")}\n"
    sb ++= "\n" + (
      if !sweepCapped(r) && realized == r.entries.size then completeMarker(scope)
      else
        s"CENSUS INCOMPLETE in scope $scope (capped=${sweepCapped(r)}, realized $realized/${r.entries.size})"
    ) + "\n"
    sb.toString

  def main(args: Array[String]): Unit =
    require(args.nonEmpty, "usage: CensusScoped k [scope] [i:j:... ...]")
    val dir                  = java.nio.file.Path.of("target", "census")
    java.nio.file.Files.createDirectories(dir)
    def say(s: String): Unit = synchronized { println(s); System.out.flush() }
    val t0                   = System.currentTimeMillis
    def dt                   = (System.currentTimeMillis - t0) / 1000

    val k        = args(0).toInt
    val scope    = args.lift(1).map(_.toInt).getOrElse(96)
    val explicit = args.drop(2).toVector.map(a => a.split(":").toVector.map(_.toInt).sorted)
    val flags0   = MonoShell.Flags()
    val kSets    =
      if explicit.nonEmpty then explicit
      else
        say(s"[${dt}s] deriving the fair $k-sets (the KSetShell sweep)...")
        KSetShell.fairKSets(k, flags0).filter(_.fair).map(_.kSet)
    say(s"[${dt}s] scoped k = $k census (union <= $scope) over ${kSets.size} $k-sets")

    // pre-warm every shared memoized structure sequentially (the parallel pass must only read)
    val speciesUsed = kSets.flatten.distinct.sorted
    for i <- speciesUsed do
      val sym = symmetryOf(i)
      val nf  = subgroupsOfSpecies(i).size
      say(s"[${dt}s] warmed ${SpeciesCorona.label(i)}: chambers ${sym.cx.chambers.size}, foldings $nf")
    for sps <- kSets do PairRealization.tablesOf(PairPatterns.ctxOf(sps))
    say(s"[${dt}s] warmed $k-set tables")

    val par  = sys.props.get("census.par").map(_.toInt).getOrElse(1).max(1)
    val out  = new java.util.concurrent.ConcurrentHashMap[Vector[Int], KSetResult]
    val pool = java.util.concurrent.Executors.newFixedThreadPool(par)
    val done = new java.util.concurrent.CountDownLatch(kSets.size)
    for sps <- kSets do
      pool.submit(new Runnable:
        def run(): Unit =
          try out.put(sps, runKSet(sps, scope, m => say(s"[${dt}s] $m")))
          catch case e: Throwable => say(s"[${dt}s] == FAILED ${sps.mkString(":")}: $e ==")
          finally done.countDown())
    done.await()
    pool.shutdown()

    val sb       = new StringBuilder
    sb ++= s"k = $k -- the SCOPED k = $k census with standalone realization\n"
    sb ++= s"per fair $k-set: every folding tuple with union <= $scope chambers, uncapped in scope;\n"
    sb ++= "minimal symbols deduped by canonical key; EVERY symbol through realizeK (symbol-driven rigid\n"
    sb ++=
      "development + periodization certificate, escalating harvest). Beyond-scope tail: CensusTails, CensusBands.\n\n"
    var total    = 0
    var realized = 0
    for sps <- kSets do
      Option(out.get(sps)) match
        case None    => sb ++= s"${sps.map(SpeciesCorona.label).mkString(" ~ ")}  FAILED\n"
        case Some(r) =>
          total += r.entries.size
          realized += r.entries.count(_._2.realized)
          val cap = r.stats.exists(_.capped) || r.entries.exists((_, v) => !v.realized && v.capped)
          sb ++= f"${r.sps.map(SpeciesCorona.label).mkString(" ~ ")}%-100s asm ${r.stats.size}%5d " +
            f"skip ${r.skippedCount}%7d sym ${r.entries.size}%3d real ${r.entries.count(_._2.realized)}%3d" +
            (if cap then "  CAPPED\n" else "\n")
          for (e, v) <- r.entries do
            sb ++=
              s"    symbol ${e.sym.size}ch at ${e.folds.map(_.map(_.size).mkString("x")).mkString(",")}: " +
                (if v.realized then s"REALIZED assembly ${v.assembly + 1}"
                 else s"NOT REALIZED capped=${v.capped}") + "\n"
    sb ++= s"\nTOTAL minimal k = $k symbols in scope: $total, realized: $realized\n"
    val name     =
      if explicit.nonEmpty then s"symbols-k$k-partial-${explicit.size}.txt" else s"symbols-k$k.txt"
    java.nio.file.Files.writeString(dir.resolve(name), sb.toString)
    say(s"\n[${dt}s] TOTAL $total symbols, $realized realized over ${kSets.size} $k-sets -> $dir/$name")
