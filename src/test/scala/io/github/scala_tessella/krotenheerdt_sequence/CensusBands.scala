package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.MonoShell

import io.github.scala_tessella.research_core.SpeciesCorona
import io.github.scala_tessella.research_core.StarFoldings

import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}
import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionAutosOf, unionOf}
import io.github.scala_tessella.research_core.SymbolCatalog.*
import SymbolRealizationFilter.{realizeK, Verdict}

/** THE BAND SWEEP: per fair k-set, every folding tuple with union in the BAND (lo, hi] chambers, swept
  * EXHAUSTIVELY under the k-part deck-conjugation lex-leader (`unionAutosOf`, as in `CensusTails`), crossless
  * tuples skipped; every minimal symbol found is deduped by canonical key and pushed through `realizeK`.
  * Under the B(k) = 36k ceiling conjecture the band (128, 160] at k = 4 contains EVERY remaining discovery of
  * the row; the residue beyond is swept the same way. Resumable driver built in: per-set certs
  * `band-k<k>-<lo>-<hi>-<i>-<j>-....txt` with canonical key lines and a band-stamped completion marker; sets
  * with a complete cert are SKIPPED; largest bands first; -Dcensus.par=N, -Dcensus.skip, -Dcensus.dry as in
  * the census driver. Summary `bands-k<k>-<lo>-<hi>-summary.txt` on non-explicit runs.
  *
  * Args: k, lo, hi, then optional explicit k-sets "i:j:..." (validation runs — no summary). Run: nohup sbt
  * -Dcensus.par=8 "Test/runMain io.github.scala_tessella.krotenheerdt_sequence.CensusBands 4 128 160" > <log>
  * 2>&1 & — plain log file, never pipe to tail.
  */
object CensusBands:

  final case class BandResult(
      sps: Vector[Int],
      tuples: Int,
      crossless: Int,
      entries: Vector[(KEntry, Verdict)],
      sweepCapped: Boolean,
      inBand: Long = 0L,
      connbad: Long = 0L
  )

  /** The band's subgroup tuples (one subgroup per role, lo < union <= hi), smallest unions first. */
  def bandTuples(sps: Vector[Int], lo: Int, hi: Int): Vector[Vector[Set[StarFoldings.Perm]]] =
    val chs = sps.map(i => symmetryOf(i).cx.chambers.size)
    sps.indices
      .foldLeft(Vector((Vector.empty[Set[StarFoldings.Perm]], 0))) { (acc, r) =>
        for
          (subs, ch) <- acc
          sub        <- subgroupsOfSpecies(sps(r))
          total       = ch + chs(r) / sub.size
          if total <= hi // prune: unions only grow
        yield (subs :+ sub, total)
      }
      .filter((_, ch) => ch > lo && ch <= hi)
      .sortBy(_._2)
      .map(_._1)

  private[krotenheerdt_sequence] def partsConnected(fs: Vector[StarFoldings.Folded]): Boolean =
    def compatible(fa: StarFoldings.Folded, fb: StarFoldings.Folded) =
      fa.cell.indices.exists(c =>
        fb.cell.indices.exists(d =>
          fa.m01(c) == fb.m01(d) && fa.m23(c) == fb.m23(d) && fa.cell(c) == fb.cell(d)
        )
      )
    val reached                                                      = collection.mutable.Set(0)
    var grew                                                         = true
    while grew do
      grew = false
      for i <- fs.indices if !reached(i) && reached.exists(j => compatible(fs(i), fs(j))) do
        reached += i
        grew = true
    reached.size == fs.size

  /** The whole band of one fair k-set: lex-leader sweep, minimal symbols realized. */
  def runKSet(sps: Vector[Int], lo: Int, hi: Int, say: String => Unit): BandResult =
    val tag                                             = sps.map(SpeciesCorona.label).mkString(" ~ ")
    val flags                                           = MonoShell.Flags()
    val syms                                            = sps.map(symmetryOf)
    // GATED + STREAMING when the gate is on: `bandTuples` materialises the whole band, and the k = 6 band
    // (128, 216] holds 2.16 BILLION tuples, so the old path cannot run there at all. ConnGate.gatedTuples
    // walks it by bounded DFS and hands back only the conn-OK survivors — 51.8 M of that 2.16 B — which is
    // sound because conn-BAD implies sigma0-empty, so no symbol can be lost. The certificate says so.
    val gated                                           = ConnGate.enabled
    // GATED + STREAMING when the gate is on. `bandTuples` materialises the whole band, and the k = 6 band
    // (128, 216] is 2.16 BILLION tuples — 181 M on its biggest set — so the old path cannot run there at
    // all. ConnGate walks it by bounded DFS and keeps only the conn-OK survivors, PACKED one Long each
    // (union in the high bits, one byte per canonical subgroup index), which is sound because conn-BAD
    // implies sigma0-empty, so no symbol can be lost. The certificate says so, and names the tables.
    val (packed, inBand, connbadN)                      =
      if gated then ConnGate.gatedPacked(sps, lo, hi) else (Array.empty[Long], 0L, 0L)
    val plain                                           = if gated then Vector.empty else bandTuples(sps, lo, hi)
    val subsOf                                          = sps.map(ConnGate.canonicalSubs)
    val k                                               = sps.size
    val nTuples                                         = if gated then packed.length else plain.size
    // one tuple's foldings, built on demand: the sweep never holds more than the one it is on
    def subsAt(pi: Int): Vector[Set[StarFoldings.Perm]] =
      if gated then
        val w = packed(pi)
        sps.indices.toVector.map(r => subsOf(r)(ConnGate.packedIndex(w, k, r)))
      else plain(pi)
    say(s"== start $tag: $nTuples band tuples ($lo, $hi]" +
      (if gated then s" of $inBand in band, $connbadN theorem-skipped ==" else " =="))
    val out                                             = collection.mutable.LinkedHashMap.empty[Vector[Int], KEntry]
    var crossless                                       = 0
    var capped                                          = false
    for pi <- 0 until nTuples do
      val subs = subsAt(pi)
      val fs   = sps.indices.toVector.map(r => fold(syms(r), subs(r)))
      if !partsConnected(fs) then crossless += 1
      else
        val autos        = unionAutosOf(sps.indices.toVector.map(r => (syms(r), subs(r), fs(r))))
        val u            = unionOf(fs)
        val (sols, capd) = enumerateSigma0(u, log = m => say(s"[$tag]$m"), autos = autos)
        capped ||= capd
        var minimal      = 0
        var fresh        = 0
        for s0 <- sols do
          val s = symOf(u, sps, s0)
          if isMinimal(s) then
            minimal += 1
            val key = canonicalKey(s)
            out.get(key) match
              case None    =>
                out(key) = KEntry(sps, s, Vector(subs))
                fresh += 1
              case Some(e) =>
                if !e.folds.contains(subs) then out(key) = e.copy(folds = e.folds :+ subs)
        if fresh > 0 || capd || (pi + 1) % 500 == 0 then
          say(
            s"[$tag]  tuple ${pi + 1}/$nTuples |H|=${subs.map(_.size).mkString(",")} " +
              s"chambers=${fs.map(_.size).mkString("+")}: ${sols.size} reps, $minimal minimal, " +
              s"$fresh new (total ${out.size})" + (if capd then " CAPPED" else "")
          )
    say(s"== $tag: ${out.size} minimal band symbols, realizing each ==")
    val budget                                          = sys.props.get("census.budget").map(_.toInt).getOrElse(200000)
    val verdicts                                        = out.values.toVector.map { e =>
      val v = realizeK(e, flags, branchBudget = budget, log = m => say(s"[$tag]$m"))
      say(
        s"[$tag] ${e.sym.size}-chamber band symbol: " +
          (if v.realized then s"REALIZED (assembly ${v.assembly + 1})"
           else s"NOT REALIZED (candidates ${v.candidates}, capped ${v.capped})")
      )
      (e, v)
    }
    say(s"== done $tag: ${verdicts.count(_._2.realized)}/${verdicts.size} realized ==")
    BandResult(sps, nTuples, crossless, verdicts, capped, inBand, connbadN)

  /** Tuples with union <= lo — the band count's lower cut. `lo` can be below every union, and `inScopeCount`
    * then returns 1 for the empty prefix, so the guard is on the smallest possible union.
    */
  private[krotenheerdt_sequence] def inScopeCount0(sps: Vector[Int], lo: Int): Long =
    val least = sps.map(i => symmetryOf(i).cx.chambers.size / subgroupsOfSpecies(i).map(_.size).max).sum
    if lo < least then 0L else CensusScoped.inScopeCount(sps, lo)

  def certOf(dir: java.nio.file.Path, sps: Vector[Int], lo: Int, hi: Int): java.nio.file.Path =
    dir.resolve(s"band-k${sps.size}-$lo-$hi-${sps.mkString("-")}.txt")

  def completeMarker(lo: Int, hi: Int): String =
    s"BAND COMPLETE ($lo, $hi] (uncapped, all realized)"

  def certText(lo: Int, hi: Int, r: BandResult): String =
    val realized = r.entries.count(_._2.realized)
    val sb       = new StringBuilder
    sb ++= s"k = ${r.sps.size} band sweep -- ${r.sps.map(SpeciesCorona.label).mkString(" ~ ")}\n"
    sb ++= s"species ${r.sps.mkString(":")}  band ($lo, $hi]\n"
    sb ++= "every folding tuple with union in the band, k-part deck-conjugation lex-leader, crossless\n"
    sb ++=
      "skipped; minimal symbols deduped by canonical key, each through realizeK; key lines parseable.\n\n"
    sb ++= f"tuples ${r.tuples}%7d crossless ${r.crossless}%7d connbad ${r.connbad}%9d " +
      f"sym ${r.entries.size}%3d real $realized%3d" + (if r.sweepCapped then "  CAPPED\n" else "\n")
    if r.connbad > 0 then
      sb ++= f"\nOF THE ${r.inBand}%d FOLDING TUPLES IN THE BAND, ${r.connbad}%d WERE NOT ASSEMBLED: they\n"
      sb ++= "are conn-BAD, and the beta1 obstruction (conn-BAD => sigma0-empty, with its pairwise\n"
      sb ++= "reduction) certifies that such a tuple carries no symbol. The completeness claimed below is\n"
      sb ++=
        "therefore conditional on that theorem. Its implementation is ConnGate, cross-validated at k = 4\n"
      sb ++= "against independent SAT refutation (271,821 table-certified tuples, zero disagreements).\n"
      sb ++= "The tables read, by content:\n"
      for (name, hash) <- ConnGate.of(r.sps).fingerprints do sb ++= s"  $name  $hash\n"
    for (e, v) <- r.entries do
      sb ++= s"symbol ${e.sym.size}ch at ${e.folds.map(_.map(_.size).mkString("x")).mkString(",")}: " +
        (if v.realized then s"REALIZED assembly ${v.assembly + 1}"
         else s"NOT REALIZED capped=${v.capped}") + "\n"
      sb ++= s"  key ${canonicalKey(e.sym).mkString(",")}\n"
    sb ++= "\n" + (
      if !r.sweepCapped && realized == r.entries.size then completeMarker(lo, hi)
      else s"BAND INCOMPLETE ($lo, $hi] (capped=${r.sweepCapped}, realized $realized/${r.entries.size})"
    ) + "\n"
    sb.toString

  def main(args: Array[String]): Unit =
    require(args.length >= 3, "usage: CensusBands k lo hi [i:j:... ...]")
    val dir                  = java.nio.file.Path.of("target", "census")
    java.nio.file.Files.createDirectories(dir)
    def say(s: String): Unit = synchronized { println(s); System.out.flush() }
    val t0                   = System.currentTimeMillis
    def dt                   = (System.currentTimeMillis - t0) / 1000

    val k        = args(0).toInt
    val lo       = args(1).toInt
    val hi       = args(2).toInt
    val explicit = args.drop(3).toVector.map(a => a.split(":").toVector.map(_.toInt).sorted)
    val kSets    =
      if explicit.nonEmpty then explicit
      else
        say(s"[${dt}s] deriving the fair $k-sets (the admissible-set sweep)")
        io.github.scala_tessella.research_core.KSetShell
          .fairKSets(k, io.github.scala_tessella.research_core.MonoShell.Flags())
          .filter(_.fair)
          .map(_.kSet)

    def alreadyDone(sps: Vector[Int]) =
      val p = certOf(dir, sps, lo, hi)
      java.nio.file.Files.exists(p) &&
      java.nio.file.Files.readString(p).contains(completeMarker(lo, hi))
    val skip                          = sys.props
      .get("census.skip")
      .map(_.split(",").toVector.map(_.split("[-:]").toVector.map(_.toInt).sorted).toSet)
      .getOrElse(Set.empty)
    val (done, rest)                  = kSets.partition(alreadyDone)
    val todo                          = rest.filterNot(skip)
    for sps <- done do say(s"[${dt}s] SKIP ${sps.mkString(":")} (band cert already complete)")
    for sps <- rest if skip(sps) do say(s"[${dt}s] SKIP ${sps.mkString(":")} (census.skip)")
    say(s"[${dt}s] band sweep: k = $k ($lo, $hi], ${todo.size} of ${kSets.size} $k-sets to run")

    // pre-warm every shared memoized structure sequentially (the parallel pass must only read)
    val speciesUsed = kSets.flatten.distinct.sorted
    for i <- speciesUsed do
      val sym = symmetryOf(i)
      val nf  = subgroupsOfSpecies(i).size
      say(s"[${dt}s] warmed ${SpeciesCorona.label(i)}: chambers ${sym.cx.chambers.size}, foldings $nf")

    // COUNTED, NOT MATERIALISED: the k = 6 band holds 2.16 billion tuples, so the ordering key is the
    // difference of two DP counts (union <= hi minus union <= lo) rather than a built list.
    val ordered =
      todo.map(sps => (sps, CensusScoped.inScopeCount(sps, hi) - inScopeCount0(sps, lo))).sortBy(-_._2)
    say(f"[${dt}s] total band tuples over ${ordered.size} sets: ${ordered.map(_._2).sum}%,d")
    for (sps, n) <- ordered.take(10) do
      say(s"[${dt}s] biggest bands: ${sps.mkString(":")} $n tuples")
    if sys.props.get("census.dry").contains("true") then
      say(s"[${dt}s] dry run: stopping before the sweep")
      return

    for (sps, n) <- todo.zipWithIndex do
      PairRealization.tablesOf(PairPatterns.ctxOf(sps))
      if (n + 1) % 25 == 0 then say(s"[${dt}s] warmed ${n + 1}/${todo.size} $k-set tables")
    say(s"[${dt}s] warmed ${todo.size} $k-set tables")

    val par      = sys.props.get("census.par").map(_.toInt).getOrElse(1).max(1)
    val pool     = java.util.concurrent.Executors.newFixedThreadPool(par)
    val doneGate = new java.util.concurrent.CountDownLatch(ordered.size)
    val finished = new java.util.concurrent.atomic.AtomicInteger
    for (sps, _) <- ordered do
      pool.submit(new Runnable:
        def run(): Unit =
          val tag = sps.mkString(":")
          try
            val r = runKSet(sps, lo, hi, m => say(s"[${dt}s] $m"))
            java.nio.file.Files.writeString(certOf(dir, sps, lo, hi), certText(lo, hi, r))
            say(s"[${dt}s] == band done $tag: sym ${r.entries.size} " +
              s"real ${r.entries.count(_._2.realized)} (${finished.incrementAndGet()}/${ordered.size}) ==")
          catch case e: Throwable => say(s"[${dt}s] == FAILED $tag: $e ==")
          finally doneGate.countDown())
    doneGate.await()
    pool.shutdown()

    if explicit.nonEmpty then say(s"[${dt}s] explicit band run done (no summary; per-set certs written)")
    else
      val statRe     =
        """tuples\s+(\d+) crossless\s+(\d+)(?: connbad\s+\d+)? sym\s+(\d+) real\s+(\d+)( {2}CAPPED)?""".r
      val sb         = new StringBuilder
      sb ++= s"k = $k -- the BAND SWEEP ($lo, $hi] over the fair $k-sets\n"
      sb ++= "per set: every folding tuple with union in the band, k-part deck-conjugation lex-leader;\n"
      sb ++= s"per-set certs band-k$k-$lo-$hi-*.txt carry the canonical keys.\n\n"
      var total      = 0
      var realized   = 0
      var incomplete = 0
      for sps <- kSets do
        val tag  = sps.map(SpeciesCorona.label).mkString(" ~ ")
        val p    = certOf(dir, sps, lo, hi)
        val text = if java.nio.file.Files.exists(p) then java.nio.file.Files.readString(p) else ""
        statRe.findFirstMatchIn(text) match
          case None    =>
            incomplete += 1
            sb ++= f"$tag%-100s MISSING (failed or not yet run)\n"
          case Some(m) =>
            total += m.group(3).toInt
            realized += m.group(4).toInt
            val ok = text.contains(completeMarker(lo, hi))
            if !ok then incomplete += 1
            sb ++= f"$tag%-100s tuples ${m.group(1).toInt}%7d crossless ${m.group(2).toInt}%7d " +
              f"sym ${m.group(3).toInt}%3d real ${m.group(4).toInt}%3d" +
              (if ok then "\n" else "  INCOMPLETE\n")
      sb ++= s"\nTOTAL band minimal symbols: $total, realized: $realized" +
        (if incomplete > 0 then s"  (NOT exhaustive: $incomplete sets incomplete)\n"
         else s"  (every fair $k-set band-exhaustive over ($lo, $hi])\n")
      java.nio.file.Files.writeString(dir.resolve(s"bands-k$k-$lo-$hi-summary.txt"), sb.toString)
      say(s"\n[${dt}s] band sweep: $total symbols, $realized realized, $incomplete incomplete " +
        s"-> $dir/bands-k$k-$lo-$hi-summary.txt")
