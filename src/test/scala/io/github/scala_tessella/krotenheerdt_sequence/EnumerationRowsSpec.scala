package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StripStacking.*
import java.nio.file.{Files, Path}

/** THE ENUMERATION ROWS: the exact enumeration of the stacking words, row by row, over the hexagon periods 2,
  * 3 and 4 with segments of at most 2k layers and the chains run to closure — a chain closes when the label
  * segment returns, within 24 segments by the paper's level-orbit lemma (the stack period closes within four,
  * the layer word read on the drifting grid within 24, 16 without a period 3), so the cap is 48k layers —
  * under every cut the paper proves. Each row must return exactly its known classes (`KnownRows`: tag,
  * chambers, species, k), no more and no fewer — at k = 8 the two 8-uniform classes, at k = 9 and 10 none.
  * Among them the two-direction words, those with rows of both axes, are the row W_k of the sequence; the
  * others are the prismatic lifts that are words. Every run writes `certs/enumeration-k<k>.txt`: its classes
  * with a word each, the count of two-direction words, and the work done.
  *
  * Opt-in and long: `sbt -Denumerate=8 test` (or `-Denumerate=5,6,7`; `-Denumerate` alone runs 2 to 10). On
  * twelve threads the rows take seconds (k = 2, 3), 9 minutes (k = 4) to about 3 hours (k = 10), about
  * fifteen hours for 2 to 10; `-Denumerate.threads=N` sets the parallelism (default: every processor).
  */
class EnumerationRowsSpec extends AnyFlatSpec with Matchers:

  private def rows: Vector[Int] =
    sys.props("enumerate").split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector match
      case Vector() => (2 to 10).toVector
      case chosen   => chosen

  /** W_k: the two-direction stackings of the row, the column of the paper's sequence table. */
  private val twoDirection: Map[Int, Int] =
    Map(2 -> 18, 3 -> 50, 4 -> 93, 5 -> 107, 6 -> 68, 7 -> 11, 8 -> 2, 9 -> 0, 10 -> 0)

  /** A word is two-direction when it has rows of both axes (the paper's definition). */
  private def isTwoDirection(r: KnownRows.Row): Boolean =
    val axes = r.layers.collect { case Layer.Tri(ax, _, _) => ax }.toSet
    axes.size == 2

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
        48 * k,
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
      val two    = got.count(isTwoDirection)
      val cert   = StringBuilder()
      cert ++=
        s"THE STACKING ENUMERATION, ROW k = $k: periods 2, 3, 4, segments <= ${2 *
            k}, chains to closure (<= 24 segments, words <= ${48 * k})\n"
      cert ++=
        s"segments ${stats.nodes}, candidates ${stats.candidates}, builds ${stats.survivors}, ${secs}s on $threads threads\n"
      cert ++= s"${got.size} classes with $k distinct species (known: ${known.size})\n"
      cert ++= s"two-direction words W_$k = $two, lifts that are words ${got.size - two}\n\n"
      for r <- got do cert ++= s"${r.chambers}ch  key ${r.tag}  ${r.species}  word [${r.word}]\n"
      Files.createDirectories(Path.of("certs"))
      Files.writeString(Path.of("certs", s"enumeration-k$k.txt"), cert.toString): Unit
      say(s"k = $k: ${got.size} classes, W = $two, in ${secs}s")
      withClue(s"row k = $k: "):
        got.map(r => (r.tag, r.chambers, r.species)).toSet shouldBe
          known.map(r => (r.tag, r.chambers, r.species)).toSet
        twoDirection.get(k).foreach(w => two shouldBe w)
