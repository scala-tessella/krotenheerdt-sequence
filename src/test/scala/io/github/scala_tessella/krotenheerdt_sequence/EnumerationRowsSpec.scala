package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StripStacking.*
import java.nio.file.{Files, Path}

/** THE ENUMERATION ROWS: the exact enumeration of the stacking words, row by row, over the hexagon periods 2,
  * 3 and 4 with segments of at most 2k layers and words of at most 8k (the paper's level-orbit bound), under
  * every cut the paper proves. Each row must return exactly its known classes (`KnownRows`: tag, chambers,
  * species, k), no more and no fewer — at k = 8 the two 8-uniform classes, at k = 9 and 10 none; the rows 1
  * to 4 are the census classes that are stacking words, the census itself being `SymbolK*CensusSpec`. Every
  * run writes `certs/enumeration-k<k>.txt`: its classes with a word each, and the work done.
  *
  * Opt-in and long: `sbt -Denumerate=8 test` (or `-Denumerate=5,6,7`; `-Denumerate` alone runs 4 to 10). On
  * twelve threads the rows take 9 minutes (k = 4) to about 4 hours (k = 10), about sixteen hours for 5 to 10;
  * `-Denumerate.threads=N` sets the parallelism (default: every processor).
  */
class EnumerationRowsSpec extends AnyFlatSpec with Matchers:

  private def rows: Vector[Int] =
    sys.props("enumerate").split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector match
      case Vector() => (4 to 10).toVector
      case chosen   => chosen

  private def say(s: String): Unit = synchronized { println(s); System.out.flush() }

  "the enumeration" should "return exactly the known classes of every row asked (-Denumerate)" in:
    assume(OptIn.enabled("enumerate"))
    val threads =
      sys.props.get("enumerate.threads").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    for k <- rows do
      val t0     = System.currentTimeMillis
      val oracle = StripEnumeration.SpeciesOracle()
      val found  = java.util.concurrent.ConcurrentHashMap[String, KnownRows.Row]()
      val stats  = StripEnumeration.enumerate(
        k,
        Set(2, 3, 4),
        2 * k,
        8 * k,
        oracle,
        (w, _) => KnownRows.classOf(k, w).foreach(r => found.putIfAbsent(r.tag, r): Unit),
        threads,
        3,
        Some(m => say(s"k = $k $m"))
      )
      val secs   = (System.currentTimeMillis - t0) / 1000
      import scala.jdk.CollectionConverters.*
      val got    = found.values.asScala.toVector.sortBy(r => (r.chambers, r.tag))
      val known  = KnownRows.row(k)
      val cert   = StringBuilder()
      cert ++=
        s"THE STACKING ENUMERATION, ROW k = $k: periods 2, 3, 4, segments <= ${2 * k}, words <= ${8 * k}\n"
      cert ++=
        s"segments ${stats.nodes}, candidates ${stats.candidates}, builds ${stats.survivors}, ${secs}s on $threads threads\n"
      cert ++= s"${got.size} classes with $k distinct species (known: ${known.size})\n\n"
      for r <- got do cert ++= s"${r.chambers}ch  key ${r.tag}  ${r.species}  word [${r.word}]\n"
      Files.createDirectories(Path.of("certs"))
      Files.writeString(Path.of("certs", s"enumeration-k$k.txt"), cert.toString): Unit
      say(s"k = $k: ${got.size} classes in ${secs}s")
      withClue(s"row k = $k: "):
        got.map(r => (r.tag, r.chambers, r.species)).toSet shouldBe
          known.map(r => (r.tag, r.chambers, r.species)).toSet
