package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StripStacking.*
import StripStacking.Layer.*
import StripSymmetry.*
import StripEnumeration.*

/** THE CERTIFICATES OF THE STACKING LEMMAS, guarded: `sbt -Dcertificates "testOnly *StripCertificatesSpec"`
  * (about four minutes; the default suite skips them). The star kinds at a base row's lower level (the
  * single-period lemma), the species reachable at a junction (the vanishing bound), the junction dictionary
  * of period-2 words (the seven-species theorem), and the k = 1 search over the periods 2 and 3 together.
  */
class StripCertificatesSpec extends AnyFlatSpec with Matchers:

  /** The star kinds at a base row's lower level, certified: with a uniform layer below (cubes, a plain row of
    * either axis, with or without a plain same-axis row under it) the vertices of the level are corners — 2
    * per period across, one species — and between vertices — p − 2 per period, one species. Over the upper
    * row of a pair of the other axis with hexagons every q the kinds multiply with the top-edge (2 per q) and
    * plain (q − 2) vertices below, except that "corner over plain" and "between over a top edge" are ONE
    * species (the two stars are exchanged by the rotation swapping the axes and flipping the stack): three
    * species when p, q ≥ 3. Counts are per translation cell of the window's build.
    */
  "the star kinds at a base row's lower level" should
    "be corner and between above, top edge and plain below, one species per kind" in {
      assume(OptIn.enabled("certificates"))
      def sortedCounts(cs: Map[Set[Int], Int]) = cs.values.toVector.sorted
      val table                                = collection.mutable.ArrayBuffer.empty[String]
      def row(below: String, base: String)     =
        val w       = parseWord(s"$below $base")
        val (a, cs) = interfaceSpeciesCounts(w)
        val at      = cs.getOrElse(1, Map.empty)
        table += f"$below%-12s | $base%-10s  cell $a%3d  ${at.toVector.sortBy(_._2).map((s, n) =>
            s"${s.mkString(",")}:$n"
          ).mkString("  ")}"
        (a, at)
      for p <- Seq(2, 3, 4); o <- 0 until p do
        val base = s"Tu{$o/$p} Tu"
        for below <- Seq("C C", "C Tu", "Tu Tu", "C Tw", "Tu Tw", "Tw Tw") do
          val (a, at) = row(below, base)
          withClue(s"$below $base: ") {
            at.keySet.flatten should not contain -1
            val expected = if p == 2 then Vector(a) else Vector(2 * a / p, (p - 2) * a / p)
            sortedCounts(at) shouldBe expected.sorted
          }
        for q <- Seq(2, 3, 4); o2 <- 0 until q do
          val (a, at) = row(s"Tw{$o2/$q} Tw", base)
          withClue(s"Tw{$o2/$q} Tw $base: ") {
            at.keySet.flatten should not contain -1
            val u        = a / (p * q)
            val expected =
              if p == 2 && q == 2 then Vector(a)
              else if p == 2 then Vector(2 * 2 * u, 2 * (q - 2) * u)
              else if q == 2 then Vector(2 * 2 * u, (p - 2) * 2 * u)
              else Vector(2 * 2 * u, (2 * (q - 2) + (p - 2) * 2) * u, (p - 2) * (q - 2) * u)
            sortedCounts(at) shouldBe expected.sorted
          }
      println(table.mkString("\n"))
    }

  /** The species a stacking word over the alphabet of periods 2, 3, 4 can carry at a junction: the union over
    * every consistent three-layer context (the layer below, the two layers meeting the junction; the fourth
    * layer of the window is the forced carrier or a cube layer). Their number bounds the k of any word over
    * the alphabet, hence the vanishing point of the two-direction count W(k).
    */
  "the species reachable at a junction" should "be a fixed set over the alphabet of periods 2, 3, 4" in {
    assume(OptIn.enabled("certificates"))
    val ls                       = letters(Set(2, 3, 4))
    val o                        = SpeciesOracle()
    def carrier(l: Layer): Layer = l match
      case Layer.Tri(ax, hx, _) if hx.nonEmpty => Layer.Tri(ax, Set.empty, 1)
      case _                                   => Layer.Cubic
    val windows                  = for l0 <- ls; l1 <- ls; l2 <- ls yield Vector(l0, l1, l2, carrier(l2))
    val ok                       =
      windows.filter(w => prefixConsistent(w) && consistent(Vector(Layer.Cubic) ++ w ++ Vector(Layer.Cubic)))
    val reached                  = collection.mutable.Map.empty[Int, Int] // species -> windows carrying it
    var unread                   = 0
    for w <- ok do
      val sp = o.at(w)
      if sp.contains(-1) then unread += 1
      for x <- sp if x >= 0 do reached(x) = reached.getOrElse(x, 0) + 1
    println(s"windows ${windows.size}, consistent ${ok.size}, unreadable $unread")
    println(reached.toVector.sorted.map((x, n) => s"$x:$n").mkString("reached ", "  ", ""))
    unread shouldBe 0
    // the two #2 arrangements of the cube species, 17 and 26, occur at no junction: at most 11 species in a word
    reached.keySet shouldBe Set(13, 16, 18, 19, 20, 21, 25, 27, 28, 29, 30)
    // per sector (the periods each axis may use under the single-period lemma), the species reachable there
    def periodsOf(l: Layer)      = l match
      case Layer.Tri(ax, hx, p) if hx.nonEmpty => Some((ax, p))
      case _                                   => None
    val sectors                  =
      for pu <- Seq(Set(2), Set(3), Set(4), Set(2, 4)); pw <- Seq(Set(2), Set(3), Set(4), Set(2, 4))
      yield (pu, pw)
    for (pu, pw) <- sectors do
      val inSector = ok.filter(_.forall(l => periodsOf(l).forall((ax, p) => if ax then pu(p) else pw(p))))
      val sp       = inSector.flatMap(o.at).filter(_ >= 0).toSet
      println(
        s"sector u ${pu.toVector.sorted.mkString(",")} w ${pw.toVector.sorted.mkString(",")}: ${sp.size} species ${sp.toVector.sorted.mkString(" ")}"
      )
  }

  /** The junction dictionary of period-2 words, certified: every junction kind — which layer types meet it,
    * same or other axis, aligned or offset hexagon columns, a kagome pair's own mid-level — carries exactly
    * one species, as the theorem on period-2 words uses.
    */
  "the junction kinds of period-2 words" should "each carry one species" in {
    assume(OptIn.enabled("certificates"))
    val ls                       = letters(Set(2))
    val o                        = SpeciesOracle()
    def carrier(l: Layer): Layer = l match
      case Layer.Tri(ax, hx, _) if hx.nonEmpty => Layer.Tri(ax, Set.empty, 1)
      case _                                   => Layer.Cubic
    def hex(l: Layer)            = l match { case Layer.Tri(_, hx, _) => hx.nonEmpty; case _ => false }
    def axis(l: Layer)           = l match { case Layer.Tri(ax, _, _) => Some(ax); case _ => None }

    /** The kind of the junction between l1 and l2, given l0 below l1. */
    def kind(l0: Layer, l1: Layer, l2: Layer): String =
      val topFace = // what l1 presents upward
        if l1 == Layer.Cubic then "C"
        else if hex(l1) then "mid" // the junction is the mid-level of l1's hexagons
        else if hex(l0) && axis(l0) == axis(l1) then "hexTop"
        else "plain"
      val botFace = if l2 == Layer.Cubic then "C" else if hex(l2) then "hexBot" else "plain"
      val same    = axis(l1) == axis(l2)
      (topFace, botFace) match
        case ("mid", _)                                => "mid-level of a kagome pair"
        case ("C", "C")                                => "C|C"
        case ("C", "plain") | ("plain", "C")           => "C|plain"
        case ("C", "hexBot") | ("hexTop", "C")         => "C|hexagon face"
        case ("plain", "plain")                        => if same then "plain|plain same axis" else "plain|plain cross"
        case ("plain", "hexBot") | ("hexTop", "plain") =>
          if same then "plain|hexagon face same axis" else "plain|hexagon face cross"
        case ("hexTop", "hexBot")                      =>
          if !same then "hexagon|hexagon cross"
          else
            val a = l0.asInstanceOf[Layer.Tri].hex.head; val b = l2.asInstanceOf[Layer.Tri].hex.head
            // the grid drifts half a unit per row: the same residue two rows up is the offset column, 3.6.3.6
            if a != b then "hexagon|hexagon same axis aligned" else "hexagon|hexagon same axis offset"
        case other                                     => other.toString
    val expected                                      = Map(
      "C|C"                               -> Set(18),
      "C|plain"                           -> Set(25),
      "C|hexagon face"                    -> Set(16),
      "plain|plain same axis"             -> Set(30),
      "plain|plain cross"                 -> Set(29),
      "plain|hexagon face same axis"      -> Set(28),
      "plain|hexagon face cross"          -> Set(27),
      "hexagon|hexagon same axis aligned" -> Set(19),
      "hexagon|hexagon same axis offset"  -> Set(21),
      "hexagon|hexagon cross"             -> Set(20),
      "mid-level of a kagome pair"        -> Set(21)
    )
    val seen                                          = collection.mutable.Map.empty[String, Set[Int]]
    var windows                                       = 0
    for l0 <- ls; l1 <- ls; l2 <- ls do
      val w = Vector(l0, l1, l2, carrier(l2))
      if prefixConsistent(w) && consistent(Vector(Layer.Cubic) ++ w ++ Vector(Layer.Cubic)) then
        windows += 1
        val k  = kind(l0, l1, l2)
        val sp = o.at(w)
        withClue(s"$k at ${showLayers(w)}: ")(sp shouldBe expected(k))
        seen(k) = seen.getOrElse(k, Set.empty) ++ sp
    println(s"period-2 windows $windows; kinds ${seen.size}: " +
      seen.toVector.sortBy(_._1).map((k, s) => s"$k -> ${s.mkString(",")}").mkString("; "))
    seen.keySet shouldBe expected.keySet
  }

  "the search at k = 1" should "add the hexagonal prismatic honeycomb over the periods 2 and 3 together" in {
    assume(OptIn.enabled("certificates"))
    val o    = SpeciesOracle()
    val keys = collection.mutable.Map.empty[Vector[Int], String]
    enumerate(
      1,
      Set(2, 3),
      3,
      12,
      o,
      (w, _) =>
        symbolOfLayers(w).foreach { x =>
          if x.orbitsOf(Vector(1, 2, 3)).max == 0 then keys.getOrElseUpdate(canonicalKey(x), showLayers(w))
        },
      1,
      3,
      None
    )
    keys.values.toVector.sorted shouldBe
      Vector("C", "C Tu", "C Tu C Tw", "Tu", "Tu Tu{0/2}", "Tu Tw", "Tu{0/3} Tu{1/3} Tu{2/3}")
  }

  /** The dictionary of every level kind over the alphabet of periods 2, 3, 4: for each consistent three-layer
    * context, a structural descriptor (the layer types, periods, axes, and the relative offsets of the
    * same-axis patterns in absolute units, reduced under the across reflection) with the species at the
    * junction and their vertices per translation cell. Printed as a table for the proofs; every context must
    * be readable.
    */
  /** The facts behind the lemma on period-4 runs and the vanishing at k = 11 over every consistent
    * three-layer context of the periods 2 and 4: a period-2 and a period-4 hexagon row are never adjacent;
    * the level between the first two rows of a period-4 run carries exactly {19, 28}; a level with a third
    * row of the run below carries 19 and another set; {p6:6}#1 (13) occurs only there; and {cube:8}#1 (18)
    * only where cubes meet cubes.
    */
  "the period-4 runs" should
    "carry {19, 28} at their first level, another set with 19 at a third row, and 13 nowhere else" in {
      assume(OptIn.enabled("certificates"))
      import io.github.scala_tessella.research_core.SpeciesCorona
      SpeciesCorona.label(13) shouldBe "{p6:6}#1"
      SpeciesCorona.label(18) shouldBe "{cube:8}#1"
      SpeciesCorona.label(19) shouldBe "{p3:4 p6:4}#1"
      SpeciesCorona.label(21) shouldBe "{p3:4 p6:4}#3"
      SpeciesCorona.label(28) shouldBe "{p3:8 p6:2}#2"
      val ls                             = letters(Set(2, 4))
      val o                              = SpeciesOracle()
      def carrier(l: Layer): Layer       = l match
        case Layer.Tri(ax, hx, _) if hx.nonEmpty => Layer.Tri(ax, Set.empty, 1)
        case _                                   => Layer.Cubic
      def hexRow(l: Layer, per: Int)     = l match
        case Layer.Tri(_, hx, p) => hx.nonEmpty && p == per
        case _                   => false
      def axis(l: Layer)                 = l match { case Layer.Tri(a, _, _) => Some(a); case _ => None }
      def run4(a: Layer, b: Layer)       = hexRow(a, 4) && hexRow(b, 4) && axis(a) == axis(b)
      val windows                        = for l0 <- ls; l1 <- ls; l2 <- ls yield Vector(l0, l1, l2, carrier(l2))
      val ok                             =
        windows.filter(w =>
          prefixConsistent(w) && consistent(Vector(Layer.Cubic) ++ w ++ Vector(Layer.Cubic))
        )
      var firstLevels, thirdRows, with13 = 0
      for w <- ok do
        val Vector(l0, l1, l2, _) = w
        val sp                    = o.at(w)
        sp should not contain -1
        // a period-2 and a period-4 hexagon row are never adjacent on one axis
        val mixed                 = (hexRow(l1, 2) && hexRow(l2, 4) || hexRow(l1, 4) && hexRow(l2, 2)) && axis(l1) == axis(l2)
        mixed shouldBe false
        if run4(l1, l2) && !run4(l0, l1) then
          firstLevels += 1
          sp shouldBe Set(19, 28)
        if run4(l0, l1) && run4(l1, l2) then
          thirdRows += 1
          sp should contain(19)
          sp should not be Set(19, 28)
        if sp.contains(13) then
          with13 += 1
          (run4(l0, l1) && run4(l1, l2)) shouldBe true
        if sp.contains(18) then (l1 == Layer.Cubic && l2 == Layer.Cubic) shouldBe true
      println(s"contexts ${ok.size}: first levels $firstLevels, third rows $thirdRows, with 13 $with13")
      firstLevels should be > 0
      thirdRows should be > 0
      with13 should be > 0
    }

  "the level dictionary" should "list the species and multiplicities of every context" in {
    assume(OptIn.enabled("certificates"))
    val ls                                   = letters(Set(2, 3, 4))
    def carrier(l: Layer): Layer             = l match
      case Layer.Tri(ax, hx, _) if hx.nonEmpty => Layer.Tri(ax, Set.empty, 1)
      case _                                   => Layer.Cubic
    def typ(l: Layer): String                = l match
      case Layer.Cubic           => "C"
      case Layer.Tri(_, hx, per) => if hx.isEmpty then "P" else s"H$per"
    def ax(l: Layer): Option[Boolean]        = l match { case Layer.Tri(a, _, _) => Some(a); case _ => None }
    def descriptor(w: Vector[Layer]): String =
      val Vector(l0, l1, l2, _)                           = w
      val sameAx01                                        = ax(l0).isDefined && ax(l0) == ax(l1)
      val sameAx12                                        = ax(l1).isDefined && ax(l1) == ax(l2)
      val sameAx02                                        = ax(l0).isDefined && ax(l0) == ax(l2)
      def pos(l: Layer, rank: Int): Option[(Double, Int)] = l match
        case Layer.Tri(_, hx, per) if hx.nonEmpty => Some((hx.head + 0.5 * rank, per))
        case _                                    => None
      // absolute bottom positions of same-axis hexagon rows, the grid drifting half a unit per same-axis row
      val rows                                            = Vector(l0, l1, l2)
      val ranks                                           = rows.indices.map(i => rows.take(i).count(r => ax(r) == ax(rows(i)) && ax(r).isDefined))
      val ps                                              = rows.indices.map(i => pos(rows(i), ranks(i)))
      def rel(i: Int, j: Int): String                     =
        (ps(i), ps(j)) match
          case (Some((xi, pi)), Some((xj, pj))) if ax(rows(i)) == ax(rows(j)) =>
            val g  = BigInt(pi).gcd(BigInt(pj)).toInt
            val d  = ((xj - xi)    % g + g) % g
            val dm = ((-(xj - xi)) % g + g) % g
            f"${math.min(d, dm)}%.1f/$g" // reduced under the across reflection
          case _                                                              => "-"
      s"${typ(l0)}${
          if sameAx01 then "=" else if ax(l0).isDefined && ax(l1).isDefined then "x" else "."
        }${typ(l1)}${if sameAx12 then "=" else if ax(l1).isDefined && ax(l2).isDefined then "x" else "."}${typ(l2)}  off01 ${rel(0, 1)} off12 ${rel(1, 2)} off02 ${rel(0, 2)}"
    val rows                                 = for l0 <- ls; l1 <- ls; l2 <- ls yield Vector(l0, l1, l2, carrier(l2))
    val ok                                   =
      rows.filter(w => prefixConsistent(w) && consistent(Vector(Layer.Cubic) ++ w ++ Vector(Layer.Cubic)))
    val table                                = collection.mutable.Map.empty[String, collection.mutable.Set[String]]
    var unread                               = 0
    for w <- ok do
      val (a, cs) = interfaceSpeciesCounts(w)
      val at      = cs.getOrElse(1, Map.empty)
      if at.keySet.flatten.contains(-1) then unread += 1
      val sp      = at.toVector.sortBy(_._1.min).map((s, n) =>
        s"${s.toVector.sorted.mkString(",")}:${n * 100 / a}%"
      ).mkString(" ")
      table.getOrElseUpdate(descriptor(w), collection.mutable.Set.empty) += sp
    println(s"contexts ${ok.size}, descriptors ${table.size}, unreadable $unread")
    for (d, sps) <- table.toVector.sortBy(_._1) do
      println(f"$d%-42s  ${sps.toVector.sorted.mkString("  ||  ")}")
    unread shouldBe 0
  }
