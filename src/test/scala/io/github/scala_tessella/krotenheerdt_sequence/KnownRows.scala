package io.github.scala_tessella.krotenheerdt_sequence

import StripStacking.*
import io.github.scala_tessella.research_core.SpeciesCorona

/** The known rows of the stacking enumeration (`known-rows.tsv`, a test resource): per class its row k, the
  * key tag (32-bit hash of its canonical key), its chambers, its species labels and one stacking word. The
  * opt-in runs assert against them; the file is data the runs must reproduce, never an input to a search.
  */
object KnownRows:

  /** One class of a row. */
  final case class Row(k: Int, tag: String, chambers: Int, species: String, word: String):
    def layers: Vector[Layer] = parseWord(word)

  /** Every known class, in file order. */
  lazy val all: Vector[Row] =
    val in =
      Option(getClass.getResourceAsStream("/known-rows.tsv")).getOrElse(sys.error("known-rows.tsv missing"))
    try
      scala.io.Source.fromInputStream(in, "UTF-8").getLines().filterNot(_.startsWith("#")).filter(_.nonEmpty)
        .map { l =>
          val Array(k, tag, ch, sp, w) = l.split("\t", -1)
          Row(k.toInt, tag, ch.toInt, sp, w)
        }
        .toVector
    finally in.close()

  /** The known classes of row k (empty for k ≥ 9). */
  def row(k: Int): Vector[Row] = all.filter(_.k == k)

  /** The tag of a canonical key, as the known rows record it. */
  def keyTag(key: Vector[Int]): String = f"${key.hashCode & 0xffffffffL}%08x"

  /** The species labels of a symbol's orbits, sorted and joined as the known rows record them. */
  def speciesLabel(x: S): String =
    speciesOf(x).map(sp => sp.toVector.sorted.map(SpeciesCorona.label).mkString(" / ")).sorted.mkString(" ~ ")

  /** A class as a run finds it: the symbol of a word, when it is k-uniform Krötenheerdt. */
  def classOf(k: Int, w: Vector[Layer]): Option[Row] =
    symbolOfLayers(w).flatMap { x =>
      val orbits = x.orbitsOf(Vector(1, 2, 3)).max + 1
      val sp     = speciesOf(x)
      Option.when(orbits == k && sp.forall(_.size == 1) && sp.map(_.head).distinct.size == k)(
        Row(k, keyTag(canonicalKey(x)), x.size, speciesLabel(x), showLayers(w))
      )
    }
