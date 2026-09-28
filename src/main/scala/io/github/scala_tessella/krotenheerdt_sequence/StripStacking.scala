package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesCorona
import io.github.scala_tessella.research_core.StarFoldings
import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}
import io.github.scala_tessella.research_core.SymbolCatalog.Sym

/** STRIP STACKINGS: the constructive half of the two-direction structure theorem for the prism world. A
  * two-direction honeycomb is a periodic stack, along a third axis g, of complete layers: CUBIC layers
  * (thickness 1) and rows of unit TRIANGLES (thickness √3/2, P3 columns, the interface grid shifting by 1/2
  * across the row) extruded along one of two perpendicular horizontal axes u, w, in which groups of six
  * triangles spanning two consecutive same-axis rows may be MERGED into flat-top hexagons (P6 columns) along
  * a periodic pattern — the kagome row is the pattern with a hexagon every second position, the 6³ block the
  * pattern with a hexagon every third position in every row pair, offset by one. The letters C, Tu, Tw, Ku0,
  * Ku1, Kw0, Kw1 are the three-letter front end (`toLayers`); `Layer` words are the general model.
  *
  * The honeycomb of a word is built exactly (x, y in half-integers; g = p + q·√3/2 with p, q integers): one
  * translation period of cells (in-plane period 4 × 4 units, the word repeated until its thickness is at
  * least 3, so that no lattice vector is shorter than a cell diameter), the flags and σ₀..σ₃ by incidence,
  * the translation quotient, then the MINIMAL image by congruence quotients and a species-free canonical key.
  * Vertex orbits are identified to species through the keys of every folded star of every species
  * (`foldedStars`). `canonicalWord` quotients the alphabet by rotation, reversal, u ↔ w and the two global
  * phase flips. Fixture: `StripStackingSpec`; the enumeration of the words is `StripEnumeration`.
  */
object StripStacking:

  // ---------- exact coordinates: (2x, 2y, p, q) with g = p + q·h, h = √3/2 ----------
  type V = (Int, Int, Int, Int)
  val h: Double         = math.sqrt(3) / 2
  def gOf(v: V): Double = v._3 + v._4 * h

  enum Letter(val label: String, val dp: Int, val dq: Int, val shiftX2: Int, val shiftY2: Int):
    case C   extends Letter("C", 1, 0, 0, 0)
    case Tu  extends Letter("Tu", 0, 1, 0, 1) // extruded along x; strip across y; grid shifts by 1/2 in y
    case Tw  extends Letter("Tw", 0, 1, 1, 0)
    case Ku0 extends Letter("Ku0", 0, 2, 0, 0)
    case Ku1 extends Letter("Ku1", 0, 2, 0, 0)
    case Kw0 extends Letter("Kw0", 0, 2, 0, 0)
    case Kw1 extends Letter("Kw1", 0, 2, 0, 0)

  /** A cell: type ordinal (cube 1, P3 9, P6 10), vertices, faces as vertex-index cycles. */
  final case class Cell(kind: Int, verts: Vector[V], faces: Vector[Vector[Int]])

  /** A prism over a polygon given in the (a, g) cross-section plane, extruded along the other axis. `alongX`:
    * cross-section coordinates are (y, g) and the extrusion runs over x ∈ [x0, x0+1]; else (x, g) over y.
    */
  def prism(kind: Int, poly: Vector[(Int, Int, Int)], alongX: Boolean, a2: Int): Cell =
    // poly points: (2·coord across, p, q)
    def pt(c: (Int, Int, Int), e: Int): V =
      if alongX then (a2 + 2 * e, c._1, c._2, c._3) else (c._1, a2 + 2 * e, c._2, c._3)
    val n                                 = poly.size
    val bottom                            = poly.map(pt(_, 0))
    val top                               = poly.map(pt(_, 1))
    val verts                             = bottom ++ top
    val faces                             = Vector((0 until n).toVector, (n until 2 * n).toVector) ++
      (0 until n).map(i => Vector(i, (i + 1) % n, n + (i + 1) % n, n + i))
    Cell(kind, verts, faces)

  def cube(x2: Int, y2: Int, p: Int, q: Int): Cell =
    val v = Vector(
      (x2, y2, p, q),
      (x2 + 2, y2, p, q),
      (x2 + 2, y2 + 2, p, q),
      (x2, y2 + 2, p, q),
      (x2, y2, p + 1, q),
      (x2 + 2, y2, p + 1, q),
      (x2 + 2, y2 + 2, p + 1, q),
      (x2, y2 + 2, p + 1, q)
    )
    Cell(
      1,
      v,
      Vector(
        Vector(0, 1, 2, 3),
        Vector(4, 5, 6, 7),
        Vector(0, 1, 5, 4),
        Vector(1, 2, 6, 5),
        Vector(2, 3, 7, 6),
        Vector(3, 0, 4, 7)
      )
    )

  // ---------- the general layer model: cubic layers, triangle rows, hexagon merges ----------

  /** A layer of the stack. `Cubic` is a layer of unit cubes. `Tri(alongX, hex, period)` is one row of unit
    * triangles (thickness √3/2) extruded along x (alongX) or y, whose up-triangles have their bases on the
    * current grid; the interface grid shifts by 1/2 across the row. `hex` lists, as residues modulo `period`
    * (units across the row), the indices m of the flat-top HEXAGONS whose lower half lies in this row and
    * upper half in the next row (which must be a same-axis Tri): the hexagon centre is at across-coordinate
    * offset + 1/2 + m. Six triangles are merged into each hexagon: in this row the up-triangle m and the
    * down-triangles m−1, m; in the next row the up-triangles m−1, m and the down-triangle m−1 (indices
    * relative to the next row's own grid). Hexagons of one row pair need |m − m′| ≥ 2; a hexagon m of this
    * row and a hexagon m′ of the next row pair need m′ − m ∉ {0, −1} (the 6³ block has m′ − m = 1).
    */
  enum Layer:
    case Cubic
    case Tri(alongX: Boolean, hex: Set[Int], period: Int)
    def dp: Int = if this == Cubic then 1 else 0
    def dq: Int = if this == Cubic then 0 else 1

  /** The letter words as layers: a kagome letter is a row pair with hexagons at every second position. */
  def toLayers(word: Vector[Letter]): Vector[Layer] =
    import Letter.*
    word.flatMap {
      case C   => Vector(Layer.Cubic)
      case Tu  => Vector(Layer.Tri(true, Set.empty, 1))
      case Tw  => Vector(Layer.Tri(false, Set.empty, 1))
      case Ku0 => Vector(Layer.Tri(true, Set(0), 2), Layer.Tri(true, Set.empty, 1))
      case Ku1 => Vector(Layer.Tri(true, Set(1), 2), Layer.Tri(true, Set.empty, 1))
      case Kw0 => Vector(Layer.Tri(false, Set(0), 2), Layer.Tri(false, Set.empty, 1))
      case Kw1 => Vector(Layer.Tri(false, Set(1), 2), Layer.Tri(false, Set.empty, 1))
    }

  /** A layer word as text: C, Tu, Tw, and Tu{m1,m2/period} for rows carrying hexagons. */
  def showLayers(layers: Vector[Layer]): String =
    layers.map {
      case Layer.Cubic            => "C"
      case Layer.Tri(ax, hx, per) =>
        (if ax then "Tu" else "Tw") +
          (if hx.isEmpty then "" else s"{${hx.toVector.sorted.mkString(",")}/$per}")
    }.mkString(" ")

  def parseWord(text: String): Vector[Layer] =
    text.trim.split("\\s+").toVector.filter(_.nonEmpty).map {
      case "C"                                           => Layer.Cubic
      case t if t.startsWith("Tu") || t.startsWith("Tw") =>
        val ax = t.startsWith("Tu")
        if t.length == 2 then Layer.Tri(ax, Set.empty, 1)
        else
          val body         = t.drop(3).dropRight(1)
          val Array(ms, p) = body.split("/")
          Layer.Tri(ax, ms.split(",").map(_.trim.toInt).toSet, p.trim.toInt)
      case other                                         => throw new IllegalArgumentException(s"bad layer token $other")
    }

  /** The merge constraints of a periodic layer word: a row with hexagons is followed by a same-axis row, its
    * hexagons are ≥ 2 apart, and hexagons of consecutive row pairs do not overlap.
    */
  def consistent(layers: Vector[Layer]): Boolean =
    val n                                      = layers.size
    def hexesMod(l: Layer, mod: Int): Set[Int] = l match
      case Layer.Tri(_, hx, per) => (for m <- hx; j <- 0 until mod / per yield (m + j * per) % mod).toSet
      case _                     => Set.empty
    layers.indices.forall { i =>
      layers(i) match
        case Layer.Tri(ax, hx, per) if hx.nonEmpty =>
          val next = layers((i + 1) % n)
          next match
            case Layer.Tri(ax2, hx2, per2) if ax2 == ax =>
              val mod = lcm(per, per2)
              val a   = hexesMod(layers(i), mod)
              val b   = hexesMod(next, mod)
              a.forall(m =>
                a.forall(m2 => m == m2 || { val d = ((m - m2) % mod + mod) % mod; d >= 2 && mod - d >= 2 })
              ) &&
              a.forall(m =>
                b.forall { m2 =>
                  val d = ((m2 - m) % mod + mod) % mod; d != 0 && d != mod - 1
                }
              )
            case _                                      => false
        case _                                     => true
    }

  def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
  def lcm(a: Int, b: Int): Int = a / gcd(a, b) * b

  /** The in-plane periods (x, y) in units: 4 (a cell diameter is below 3) raised to the lcm of the hexagon
    * periods of the rows across that direction.
    */
  def periods(layers: Vector[Layer]): (Int, Int) =
    val px = layers.collect { case Layer.Tri(false, hx, per) if hx.nonEmpty => per }.foldLeft(4)(lcm)
    val py = layers.collect { case Layer.Tri(true, hx, per) if hx.nonEmpty => per }.foldLeft(4)(lcm)
    (px, py)

  /** The cells of one layer over the in-plane periods, given the bottom grid offset (ox2, oy2) and the
    * hexagons of the previous row (whose upper halves lie in this row).
    */
  def layerCells(
      l: Layer,
      prevHex: Set[Int],
      ox2: Int,
      oy2: Int,
      p: Int,
      q: Int,
      px: Int,
      py: Int
  ): Vector[Cell] =
    l match
      case Layer.Cubic                =>
        (for i <- 0 until px; k <- 0 until py yield cube(ox2 + 2 * i, oy2 + 2 * k, p, q)).toVector
      case Layer.Tri(alongX, hx, per) =>
        val o2         = if alongX then oy2 else ox2 // offset across the row
        val a2         = if alongX then ox2 else oy2 // offset along the extrusion
        val across     = if alongX then py else px // units across
        val along      = if alongX then px else py
        val mine       = (for m <- hx; j <- 0 until across / per yield (m + j * per) % across).toSet
        // triangles removed: lower halves of my hexagons (up m, down m−1, m); upper halves of the previous row's
        // hexagons (up m−1, m; down m−1), all indices modulo the across period
        def md(a: Int) = ((a % across) + across) % across
        val noUp       = mine ++ prevHex.flatMap(m => Set(md(m - 1), md(m)))
        val noDown     = mine.flatMap(m => Set(md(m - 1), md(m))) ++ prevHex.map(m => md(m - 1))
        val tris       =
          for
            k <- (0 until across).toVector
            c <- Vector(
                   if noUp(k) then None
                   else Some(Vector((o2 + 2 * k, p, q), (o2 + 2 * k + 2, p, q), (o2 + 2 * k + 1, p, q + 1))),
                   if noDown(k) then None
                   else
                     Some(Vector(
                       (o2 + 2 * k + 1, p, q + 1),
                       (o2 + 2 * k + 2, p, q),
                       (o2 + 2 * k + 3, p, q + 1)
                     ))
                 ).flatten
          yield (9, c)
        val hexes      =
          for m <- mine.toVector.sorted yield
            val c2 = o2 + 1 + 2 * m
            (
              10,
              Vector(
                (c2 - 1, p, q),
                (c2 + 1, p, q),
                (c2 + 2, p, q + 1),
                (c2 + 1, p, q + 2),
                (c2 - 1, p, q + 2),
                (c2 - 2, p, q + 1)
              )
            )
        for e <- (0 until along).toVector; (kind, c) <- tris ++ hexes yield prism(kind, c, alongX, a2 + 2 * e)

  /** The cells of one period of a layer word and the lattice vector (shift, thickness) along g. */
  def buildLayers(layers0: Vector[Layer]): (Vector[Cell], V, (Int, Int)) =
    require(consistent(layers0), "inconsistent hexagon merges")
    val (px, py) = periods(layers0)
    // repeat the word until the g-period is at least 3 units: no lattice vector shorter than a cell diameter
    val t0       = layers0.map(_.dp).sum + layers0.map(_.dq).sum * h
    val reps     = math.max(1, math.ceil(3.0 / t0 - 1e-9).toInt)
    val layers   = Vector.fill(reps)(layers0).flatten
    var ox2      = 0
    var oy2      = 0
    var p        = 0
    var q        = 0
    val out      = Vector.newBuilder[Cell]
    for i <- layers.indices do
      val l       = layers(i)
      val across  = l match { case Layer.Tri(true, _, _) => py; case _ => px }
      val prevHex = layers((i - 1 + layers.size) % layers.size) match
        case Layer.Tri(ax, hx, per) if hx.nonEmpty && l.isInstanceOf[Layer.Tri] =>
          (for m <- hx; j <- 0 until across / per yield (m + j * per) % across).toSet
        case _                                                                  => Set.empty[Int]
      out ++= layerCells(l, prevHex, ox2, oy2, p, q, px, py)
      l match
        case Layer.Cubic             => p += 1
        case Layer.Tri(alongX, _, _) =>
          q += 1
          if alongX then oy2 += 1 else ox2 += 1
    (out.result(), (ox2, oy2, p, q), (px, py))

  def build(word0: Vector[Letter]): (Vector[Cell], V) =
    val (cells, per, _) = buildLayers(toLayers(word0))
    (cells, per)

  /** Reduce a vertex modulo the lattice generated by (2px,0,0,0), (0,2py,0,0) and the g-period vector. */
  def reduce(v: V, per: V, px: Int = 4, py: Int = 4): V =
    val G  = per._3 + per._4 * h
    val n  = math.floor((gOf(v) + 1e-9) / G).toInt
    val mx = 2 * px
    val my = 2 * py
    val x  = ((v._1 - n * per._1) % mx + mx) % mx
    val y  = ((v._2 - n * per._2) % my + my) % my
    (x, y, v._3 - n * per._3, v._4 - n * per._4)

  /** The flags of the translation quotient with σ₀..σ₃, decorations, and the class map of the extra
    * translations passed to `flags` (identity when none): a congruence to quotient by before the minimal
    * image.
    */
  final case class Flags(
      s: Vector[Vector[Int]],
      m01: Vector[Int],
      m23: Vector[Int],
      cell: Vector[Int],
      classes: Vector[Int],
      vert: Vector[V]
  )

  /** The lattice vector of ONE repetition of a layer word: its total grid shift and thickness. */
  def layersVector(layers: Vector[Layer]): V =
    val sx = layers.count { case Layer.Tri(false, _, _) => true; case _ => false }
    val sy = layers.count { case Layer.Tri(true, _, _) => true; case _ => false }
    (sx, sy, layers.map(_.dp).sum, layers.map(_.dq).sum)

  def wordVector(word0: Vector[Letter]): V = layersVector(toLayers(word0))

  /** The minimal symbol of a layer word: build, flags, the quotient by the smallest in-plane translations
    * that are symmetries (the lcm of the hexagon periods across each direction, at least 1 unit) and by the
    * one-word period, then the minimal image. None if the merges are inconsistent.
    */
  def symbolOfLayers(layers: Vector[Layer]): Option[S] =
    if !consistent(layers) then None
    else
      val (cells, per, (px, py)) = buildLayers(layers)
      val tx                     = layers.collect { case Layer.Tri(false, hx, p) if hx.nonEmpty => p }.foldLeft(1)(lcm)
      val ty                     = layers.collect { case Layer.Tri(true, hx, p) if hx.nonEmpty => p }.foldLeft(1)(lcm)
      flags(cells, per, Vector((2 * tx, 0, 0, 0), (0, 2 * ty, 0, 0), layersVector(layers)), px, py).map { fl =>
        minimalImage(quotient(S(fl.s, fl.m01, fl.m23, fl.cell), fl.classes))
      }

  def symbolOf(word0: Vector[Letter]): Option[S] = symbolOfLayers(toLayers(word0))

  /** Flags of the translation quotient with σ₀..σ₃ by incidence; None if some face is not shared by two
    * cells.
    */
  def flags(
      cells: Vector[Cell],
      per: (Int, Int, Int, Int),
      translations: Vector[V] = Vector.empty,
      px: Int = 4,
      py: Int = 4
  ): Option[Flags] =
    val vid                                             = collection.mutable.Map.empty[V, Int]
    val coords                                          = collection.mutable.ArrayBuffer.empty[V]
    def id(v: V)                                        =
      val r = reduce(v, per, px, py)
      vid.getOrElseUpdate(r, { coords += r; vid.size })
    val cv                                              = cells.map(_.verts.map(id))
    // face key: reduced vertex set; edge key: reduced vertex pair
    val faceOwners                                      = collection.mutable.Map.empty[Set[Int], List[(Int, Int)]]
    for (c, ci) <- cells.zipWithIndex; (f, fi) <- c.faces.zipWithIndex do
      val key = f.map(cv(ci)).toSet
      faceOwners(key) = (ci, fi) :: faceOwners.getOrElse(key, Nil)
    if faceOwners.values.exists(_.size != 2) then return None
    // flag = (cell, face, edge position i (edge v_i v_{i+1}), end e ∈ {0,1})
    val index                                           = collection.mutable.Map.empty[(Int, Int, Int, Int), Int]
    val list                                            = Vector.newBuilder[(Int, Int, Int, Int)]
    for (c, ci) <- cells.zipWithIndex; (f, fi) <- c.faces.zipWithIndex; i <- f.indices; e <- 0 to 1 do
      index((ci, fi, i, e)) = list.knownSize
      list += ((ci, fi, i, e))
    val fl                                              = list.result()
    val n                                               = fl.size
    val s                                               = Array.fill(4)(Array.fill(n)(-1))
    val m01                                             = Array.fill(n)(0)
    val cellOf                                          = Array.fill(n)(0)
    val vc                                              = new Array[V](n)
    def vertexOf(ci: Int, fi: Int, i: Int, e: Int): Int =
      val f = cells(ci).faces(fi); cv(ci)(f((i + e) % f.size))
    def edgeOf(ci: Int, fi: Int, i: Int): Set[Int]      =
      val f = cells(ci).faces(fi); Set(cv(ci)(f(i)), cv(ci)(f((i + 1) % f.size)))
    for ((ci, fi, i, e), k) <- fl.zipWithIndex do
      val f          = cells(ci).faces(fi)
      m01(k) = f.size
      cellOf(k) = cells(ci).kind
      s(0)(k) = index((ci, fi, i, 1 - e))
      // σ₁: other edge of the face at the same vertex
      val v          = vertexOf(ci, fi, i, e)
      vc(k) = coords(v)
      val j          = if e == 0 then (i - 1 + f.size) % f.size else (i + 1) % f.size
      s(1)(k) = index((ci, fi, j, if e == 0 then 1 else 0))
      // σ₂: other face of the cell containing this edge, same vertex
      val ek         = edgeOf(ci, fi, i)
      val (fj, ij)   = (for
        fj <- cells(ci).faces.indices
        if fj != fi
        ij <- cells(ci).faces(fj).indices
        if edgeOf(ci, fj, ij) == ek
      yield (fj, ij)).head
      val ej         = if vertexOf(ci, fj, ij, 0) == v then 0 else 1
      s(2)(k) = index((ci, fj, ij, ej))
      // σ₃: the other cell on this face, same edge and vertex
      val key        = f.map(cv(ci)).toSet
      val (cj, fj3)  = faceOwners(key).find(_ != (ci, fi)).get
      val (ij3, ej3) = (for
        ij <- cells(cj).faces(fj3).indices
        if edgeOf(cj, fj3, ij) == ek
        ej <- 0 to 1
        if vertexOf(cj, fj3, ij, ej) == v
      yield (ij, ej)).head
      s(3)(k) = index((cj, fj3, ij3, ej3))
    // m23: cells around the edge = (σ₂σ₃)-orbit length
    val m23                                             = Array.fill(n)(0)
    for k <- 0 until n do
      var a = s(3)(s(2)(k)); var r = 1
      while a != k do { a = s(3)(s(2)(a)); r += 1 }
      m23(k) = r
    // classes under the extra translations: flag -> translated flag, union-find
    val parent                                          = Array.tabulate(n)(identity)
    def find(a: Int): Int                               = if parent(a) == a then a else { parent(a) = find(parent(a)); parent(a) }
    if translations.nonEmpty then
      val cellByKey          = cells.indices.map(ci => cv(ci).sorted -> ci).toMap
      def add(v: V, t: V): V = (v._1 + t._1, v._2 + t._2, v._3 + t._3, v._4 + t._4)
      for t <- translations do
        val tcell = cells.indices.map { ci =>
          val ids = cells(ci).verts.map(v => vid(reduce(add(v, t), per, px, py)))
          (cellByKey(ids.sorted), ids)
        }
        for ((ci, fi, i, e), k) <- fl.zipWithIndex do
          val (cj, ids) = tcell(ci)
          val f         = cells(ci).faces(fi)
          val fset      = f.map(ids).toSet
          val fj        = cells(cj).faces.indexWhere(g => g.map(cv(cj)).toSet == fset)
          val g         = cells(cj).faces(fj)
          val eset      = Set(ids(f(i)), ids(f((i + 1) % f.size)))
          val vset      = ids(f((i + e) % f.size))
          val ij        = g.indices.find(j => Set(cv(cj)(g(j)), cv(cj)(g((j + 1) % g.size))) == eset).get
          val ej        = if cv(cj)(g(ij)) == vset then 0 else 1
          val k2        = index((cj, fj, ij, ej))
          val (ra, rb)  = (find(k), find(k2))
          if ra != rb then parent(ra) = rb
    Some(Flags(
      s.map(_.toVector).toVector,
      m01.toVector,
      m23.toVector,
      cellOf.toVector,
      Vector.tabulate(n)(find),
      vc.toVector
    ))

  /** THE INTERFACE SPECIES ORACLE. The species at the interface plane between two consecutive layers depends
    * only on the four-layer window around it (the two layers, and the hexagons of the neighbouring row pairs
    * whose edges touch the plane). The window is padded with cubic layers (and a plain same-axis row where a
    * trailing row carries hexagons), built, and the UNFOLDED vertex stars at each interface height — the
    * ⟨σ₁,σ₂,σ₃⟩-orbits of the translation quotient — are keyed against the species table. Returns, per window
    * interface index i (between window(i) and window(i+1)), the set of species present there; the window's
    * outermost interfaces are excluded (they see the padding).
    */
  def interfaceSpecies(window: Vector[Layer]): Map[Int, Set[Int]] =
    interfaceSpeciesCounts(window)._2.view.mapValues(_.keySet.flatten).toMap

  /** The interface oracle with multiplicities: the in-plane translation cell of the window's build (its area
    * in unit squares) and, per interface, the number of vertices per cell whose unfolded star has each
    * species set (a set, since a star key may fold to several species; −1 marks an unknown star). The counts
    * are what the residue arguments of the single-period theorem uses: the corners of a base row are 2 per
    * period across, the between vertices p − 2.
    */
  def interfaceSpeciesCounts(window: Vector[Layer]): (Int, Map[Int, Map[Set[Int], Int]]) =
    val padBefore              = Vector(Layer.Cubic)
    val padAfter               = window.last match
      case Layer.Tri(ax, hx, _) if hx.nonEmpty => Vector(Layer.Tri(ax, Set.empty, 1), Layer.Cubic)
      case _                                   => Vector(Layer.Cubic)
    val padded                 = padBefore ++ window ++ padAfter
    if !consistent(padded) then return (0, Map.empty)
    val (cells, per, (px, py)) = buildLayers(padded)
    flags(cells, per, Vector.empty, px, py) match
      case None     => (px * py, Map.empty)
      case Some(fl) =>
        val x      = S(fl.s, fl.m01, fl.m23, fl.cell)
        val vo     = x.orbitsOf(Vector(1, 2, 3))
        // the g-level of every chamber's vertex, as (p, q); interface i of the window is at the bottom of padded(i+2)
        val levels = padded.scanLeft((0, 0))((acc, l) => (acc._1 + l.dp, acc._2 + l.dq))
        val out    = collection.mutable.Map.empty[Int, Map[Set[Int], Int]]
        for o <- 0 to vo.max do
          val ch  = (0 until x.size).filter(vo(_) == o).toVector
          val v   = fl.vert(ch.head)
          val lvl = (v._3, v._4)
          val i   = levels.indexOf(lvl) - 2
          if i >= 0 && i < window.size - 1 then
            val loc = ch.zipWithIndex.toMap
            val k   = starKey(
              ch.size,
              c => loc(x.s(1)(ch(c))),
              c => loc(x.s(2)(ch(c))),
              c => loc(x.s(3)(ch(c))),
              c => x.m01(ch(c)),
              c => x.m23(ch(c)),
              c => x.cell(ch(c))
            )
            val sp  = foldedStars.getOrElse(k, Set(-1))
            val m   = out.getOrElse(i, Map.empty)
            out(i) = m + (sp -> (m.getOrElse(sp, 0) + 1))
        (px * py, out.toMap)

  // ---------- species-free symbol algebra ----------
  final case class S(s: Vector[Vector[Int]], m01: Vector[Int], m23: Vector[Int], cell: Vector[Int]):
    def size: Int  = m01.size
    def toSym: Sym =
      val vo = orbitsOf(Vector(1, 2, 3))
      Sym(s(0), s(1), s(2), s(3), m01, m23, cell, vo)

    /** orbit id per chamber under the given generators */
    def orbitsOf(gens: Vector[Int]): Vector[Int] =
      val o = Array.fill(size)(-1); var id = 0
      for c <- 0 until size if o(c) < 0 do
        var front = List(c); o(c) = id
        while front.nonEmpty do
          val x = front.head; front = front.tail
          for g <- gens do
            val y = s(g)(x)
            if o(y) < 0 then { o(y) = id; front = y :: front }
        id += 1
      o.toVector

  def congruence(x: S): Option[Vector[Int]] =
    def classesFrom(d0: Int): Option[Vector[Int]] =
      val parent            = Array.tabulate(x.size)(identity)
      def find(a: Int): Int = if parent(a) == a then a else { parent(a) = find(parent(a)); parent(a) }
      var ok                = true
      var pairs             = List((0, d0))
      while pairs.nonEmpty && ok do
        val (a, b)   = pairs.head; pairs = pairs.tail
        val (ra, rb) = (find(a), find(b))
        if ra != rb then
          if x.m01(ra) != x.m01(rb) || x.m23(ra) != x.m23(rb) || x.cell(ra) != x.cell(rb) then ok = false
          else
            parent(ra) = rb
            pairs = (0 to 3).map(g => (x.s(g)(ra), x.s(g)(rb))).toList ::: pairs
      if ok then Some(Vector.tabulate(x.size)(find)) else None
    (1 until x.size).iterator.flatMap(classesFrom).nextOption()

  def quotient(x: S, cls: Vector[Int]): S =
    val reps  = cls.distinct
    val newId = reps.zipWithIndex.toMap
    val first = Array.fill(reps.size)(-1)
    for c <- cls.indices.reverse do first(newId(cls(c))) = c
    val rep   = first.toVector
    S(
      (0 to 3).toVector.map(g => rep.map(c => newId(cls(x.s(g)(c))))),
      rep.map(x.m01),
      rep.map(x.m23),
      rep.map(x.cell)
    )

  def minimalImage(x: S): S =
    var cur = x
    var go  = true
    while go do
      congruence(cur) match
        case Some(cls) => cur = quotient(cur, cls)
        case None      => go = false
    cur

  def canonicalKey(x: S): Vector[Int] =
    def codeFrom(start: Int): Vector[Int] =
      val num = Array.fill(x.size)(-1)
      val q   = collection.mutable.ArrayBuffer(start)
      num(start) = 0
      var qi  = 0
      while qi < q.size do
        val c = q(qi); qi += 1
        for g <- 0 to 3 do
          val d = x.s(g)(c)
          if num(d) < 0 then { num(d) = q.size; q += d }
      q.toVector.flatMap(c => (0 to 3).map(g => num(x.s(g)(c))) ++ Vector(x.m01(c), x.m23(c), x.cell(c)))
    // one start at a time: materialising every code is quadratic memory on large symbols
    var best: Vector[Int]                 = null
    for c <- 0 until x.size do
      val code = codeFrom(c)
      if best == null || math.Ordering.Implicits.seqOrdering[Vector, Int].lt(code, best) then best = code
    best

  /** Canonical code of a decorated 3-generator star (chambers with σ₁, σ₂, σ₃, m₀₁, m₂₃, cell). */
  def starKey(
      n: Int,
      s1: Int => Int,
      s2: Int => Int,
      s3: Int => Int,
      m01: Int => Int,
      m23: Int => Int,
      cell: Int => Int
  ): Vector[Int] =
    val gens                              = Vector(s1, s2, s3)
    def codeFrom(start: Int): Vector[Int] =
      val num = Array.fill(n)(-1)
      val q   = collection.mutable.ArrayBuffer(start)
      num(start) = 0
      var qi  = 0
      while qi < q.size do
        val c = q(qi); qi += 1
        for g <- gens do
          val d = g(c)
          if num(d) < 0 then { num(d) = q.size; q += d }
      q.toVector.flatMap(c => gens.map(g => num(g(c))) ++ Vector(m01(c), m23(c), cell(c)))
    var best: Vector[Int]                 = null
    for c <- 0 until n do
      val code = codeFrom(c)
      if best == null || math.Ordering.Implicits.seqOrdering[Vector, Int].lt(code, best) then best = code
    best

  /** Every folded star of every species, keyed: the species identification table. */
  lazy val foldedStars: Map[Vector[Int], Set[Int]] =
    val m = collection.mutable.Map.empty[Vector[Int], Set[Int]]
    for i <- io.github.scala_tessella.research_core.SpeciesEnumerator.species.indices do
      val sym = symmetryOf(i)
      for sub <- subgroupsOfSpecies(i) do
        val f = fold(sym, sub)
        val k = starKey(f.size, f.s1, f.s2, f.s3, f.m01, f.m23, f.cell)
        m(k) = m.getOrElse(k, Set.empty) + i
    m.toMap

  /** Every folded star keyed to the (species, subgroup) pairs that fold to it. */
  lazy val foldedStarSubs: Map[Vector[Int], Vector[(Int, Set[StarFoldings.Perm])]] =
    val m = collection.mutable.Map.empty[Vector[Int], Vector[(Int, Set[StarFoldings.Perm])]]
    for i <- io.github.scala_tessella.research_core.SpeciesEnumerator.species.indices do
      val sym = symmetryOf(i)
      for sub <- subgroupsOfSpecies(i) do
        val f = fold(sym, sub)
        val k = starKey(f.size, f.s1, f.s2, f.s3, f.m01, f.m23, f.cell)
        m(k) = m.getOrElse(k, Vector.empty) :+ ((i, sub))
    m.toMap

  /** The star key of one vertex orbit of a symbol. */
  def orbitStarKey(x: S, orbit: Int): Vector[Int] =
    val vo  = x.orbitsOf(Vector(1, 2, 3))
    val ch  = (0 until x.size).filter(vo(_) == orbit).toVector
    val loc = ch.zipWithIndex.toMap
    starKey(
      ch.size,
      c => loc(x.s(1)(ch(c))),
      c => loc(x.s(2)(ch(c))),
      c => loc(x.s(3)(ch(c))),
      c => x.m01(ch(c)),
      c => x.m23(ch(c)),
      c => x.cell(ch(c))
    )

  /** The species of each vertex orbit of a minimal symbol (a set: folded stars can coincide across species).
    */
  def speciesOf(x: S): Vector[Set[Int]] =
    val vo = x.orbitsOf(Vector(1, 2, 3))
    (0 to vo.max).toVector.map { o =>
      val ch  = (0 until x.size).filter(vo(_) == o).toVector
      val loc = ch.zipWithIndex.toMap
      val k   = starKey(
        ch.size,
        c => loc(x.s(1)(ch(c))),
        c => loc(x.s(2)(ch(c))),
        c => loc(x.s(3)(ch(c))),
        c => x.m01(ch(c)),
        c => x.m23(ch(c)),
        c => x.cell(ch(c))
      )
      foldedStars.getOrElse(k, Set.empty)
    }

  /** The letter symmetries: u ↔ w and the two global kagome phase flips generate a group of order 8 (the swap
    * conjugates the flips), acting letterwise; every symmetry of a stacking honeycomb induces a rotation or
    * reversal of its word composed with one of these.
    */
  val letterGroup: Vector[Letter => Letter] =
    import Letter.*
    def swap(l: Letter): Letter    =
      l match
        case Tu  => Tw
        case Tw  => Tu
        case Ku0 => Kw0
        case Ku1 => Kw1
        case Kw0 => Ku0
        case Kw1 => Ku1
        case C   => C
    def flipU(l: Letter): Letter   = l match { case Ku0 => Ku1; case Ku1 => Ku0; case o => o }
    def flipW(l: Letter): Letter   = l match { case Kw0 => Kw1; case Kw1 => Kw0; case o => o }
    val gens                       = Vector[Letter => Letter](swap, flipU, flipW)
    def table(f: Letter => Letter) = Letter.values.toVector.map(f)
    val id: Letter => Letter       = l => l
    var elems                      = Map(table(id) -> id)
    var frontier                   = elems
    while frontier.nonEmpty do
      val next  =
        for (_, f) <- frontier.toVector; g <- gens yield
          val fg: Letter => Letter = l => g(f(l))
          table(fg) -> fg
      val fresh = next.filterNot((t, _) => elems.contains(t)).toMap
      elems ++= fresh
      frontier = fresh
    elems.values.toVector

  /** Canonical word under rotation, reversal and the letter group. */
  def canonicalWord(w: Vector[Letter]): Vector[Letter] =
    val variants =
      for
        a <- Vector(w, w.reverse)
        g <- letterGroup
        b  = a.map(g)
        r <- b.indices
      yield b.drop(r) ++ b.take(r)
    variants.map(_.map(_.ordinal)).min(using math.Ordering.Implicits.seqOrdering).map(Letter.fromOrdinal)
