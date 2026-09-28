package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesCorona
import io.github.scala_tessella.research_core.StarFoldings

import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}
import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionAutosOf, unionOf}
import io.github.scala_tessella.research_core.SymbolCatalog.*

/** k ≥ 3 TAIL driver: the BEYOND-SCOPE TAIL of one fair triple — every folding tuple with union > `scope`
  * chambers, swept with the k-part DECK-CONJUGATION LEX-LEADER (`unionAutosOf`), crossless tuples
  * (part-compatibility graph disconnected) skipped outright. Any MINIMAL symbol found here whose key is not
  * in the scoped census is a tail discovery for the realization filter; none is the expected closure. Run:
  * sbt "Test/runMain io.github.scala_tessella.krotenheerdt_sequence.CensusTails i:j:k [scope]" — output to
  * target/census/tail-k3-<i>-<j>-<k>.txt.
  */
object CensusTails:

  final case class TailResult(
      sps: Vector[Int],
      tuples: Int,
      crossless: Int,
      capped: Boolean,
      symbols: Vector[Sym]
  )

  /** The whole beyond-scope tail of one fair triple; writes `target/census/tail-k3-<i>-<j>-<k>.txt` unless
    * `writeCert` is off (the census pinning spec re-derives tails without touching the certs).
    */
  def runTriple(sps: Vector[Int], scope: Int, say: String => Unit, writeCert: Boolean = true): TailResult =
    val dir = java.nio.file.Path.of("target", "census")
    if writeCert then java.nio.file.Files.createDirectories(dir)

    val syms                                                         = sps.map(symmetryOf)
    val tailTuples                                                   =
      (for
        sa  <- subgroupsOfSpecies(sps(0))
        sb  <- subgroupsOfSpecies(sps(1))
        sc  <- subgroupsOfSpecies(sps(2))
        subs = Vector(sa, sb, sc)
        chs  = sps.indices.map(r => syms(r).cx.chambers.size / subs(r).size)
        if chs.sum > scope
      yield (subs, chs.sum)).toVector.sortBy(_._2)
    def compatible(fa: StarFoldings.Folded, fb: StarFoldings.Folded) =
      fa.cell.indices.exists(c =>
        fb.cell.indices.exists(d =>
          fa.m01(c) == fb.m01(d) && fa.m23(c) == fb.m23(d) && fa.cell(c) == fb.cell(d)
        )
      )
    def partsConnected(fs: Vector[StarFoldings.Folded]): Boolean     =
      val reached = collection.mutable.Set(0)
      var grew    = true
      while grew do
        grew = false
        for i <- fs.indices if !reached(i) && reached.exists(j => compatible(fs(i), fs(j))) do
          reached += i
          grew = true
      reached.size == fs.size

    val tag = sps.map(SpeciesCorona.label).mkString(" ~ ")
    say(s"k = 3 tail of $tag: ${tailTuples.size} beyond-scope tuples (scope $scope)")

    val out       = collection.mutable.LinkedHashMap.empty[Vector[Int], Sym]
    val sb        = new StringBuilder
    sb ++= s"k >= 3 tail -- the beyond-scope tail of $tag\n"
    sb ++= s"every folding tuple with union > $scope chambers, k-part deck-conjugation lex-leader\n\n"
    var anyCapped = false
    var crossless = 0
    for ((subs, ch), pi) <- tailTuples.zipWithIndex do
      val fs = sps.indices.toVector.map(r => fold(syms(r), subs(r)))
      if !partsConnected(fs) then crossless += 1
      else
        val autos        = unionAutosOf(sps.indices.toVector.map(r => (syms(r), subs(r), fs(r))))
        val u            = unionOf(fs)
        say(
          s"tuple ${pi + 1}/${tailTuples.size} |H|=${subs.map(_.size).mkString(",")} " +
            s"chambers=${fs.map(_.size).mkString("+")} autos=${autos.size}"
        )
        val (sols, capd) = enumerateSigma0(u, log = say, autos = autos)
        anyCapped ||= capd
        var minimal      = 0
        var fresh        = 0
        for s0 <- sols do
          val s = symOf(u, sps, s0)
          if isMinimal(s) then
            minimal += 1
            val key = canonicalKey(s)
            if !out.contains(key) then
              out(key) = s
              fresh += 1
        say(s"  ${sols.size} orbit reps, $minimal minimal, $fresh new keys (total ${out.size})")
        sb ++= f"|H|=${subs.map(_.size).mkString(",")}%-12s chambers=${fs.map(_.size).mkString("+")}%-12s " +
          f"autos=${autos.size}%5d reps=${sols.size}%6d minimal=$minimal%4d new=$fresh%3d" +
          (if capd then "  CAPPED\n" else "\n")
    sb ++= s"\ncrossless tuples skipped: $crossless of ${tailTuples.size}\n"
    sb ++= s"TOTAL distinct minimal tail symbols: ${out.size}" +
      (if anyCapped then "  (CAPPED — not exhaustive)" else "  (exhaustive over the tail)") + "\n"
    for (_, s) <- out do
      sb ++= s"  ${s.size}-chamber minimal symbol (TAIL DISCOVERY — send to the realization filter)\n"
    if writeCert then
      java.nio.file.Files.writeString(dir.resolve(s"tail-k3-${sps.mkString("-")}.txt"), sb.toString)
    say(s"tail done: ${out.size} minimal symbols" +
      (if writeCert then s" -> $dir/tail-k3-${sps.mkString("-")}.txt" else ""))
    TailResult(sps, tailTuples.size, crossless, anyCapped, out.values.toVector)

  def main(args: Array[String]): Unit =
    require(args.nonEmpty, "usage: CensusTails i:j:k [scope]")
    val sps                  = args(0).split(":").toVector.map(_.toInt)
    val scope                = args.lift(1).map(_.toInt).getOrElse(96)
    def say(s: String): Unit = { println(s); System.out.flush() }
    val t0                   = System.currentTimeMillis
    def dt                   = (System.currentTimeMillis - t0) / 1000
    runTriple(sps, scope, m => say(s"[${dt}s] $m"))
