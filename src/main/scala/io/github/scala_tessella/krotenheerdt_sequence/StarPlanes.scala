package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesCorona
import io.github.scala_tessella.research_core.SpeciesEnumerator

/** JUNCTION PLANES OF A SPECIES STAR (the facts F1–F3 behind the paper's hexagon-world and separation
  * theorems). A star is the set of cells at a vertex, each given by its corner: the edge directions from the
  * vertex to its neighbours in that cell. A face at the vertex is a pair of edges of one corner whose angle
  * is an interior angle of one of the cell's polygons; a JUNCTION PLANE through the vertex is a plane in
  * which the faces of the star close up around the vertex (their angles sum to 360°) and which no cell
  * straddles; the cells then split into two sides. `junctionPlanes(i)` lists them for species i with the
  * polygon pattern in the plane and the cells of each side, prisms marked by whether their axis is the
  * plane's normal.
  */
object StarPlanes:

  type V = (Double, Double, Double)
  private def dot(a: V, b: V): Double      = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def cross(a: V, b: V): V         =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)
  private def norm(a: V): V                =
    val n = math.sqrt(dot(a, a))
    (a._1 / n, a._2 / n, a._3 / n)
  def sameLine(a: V, b: V): Boolean        = math.abs(math.abs(dot(a, b)) - 1) < 1e-6
  private def sameDir(a: V, b: V)          = math.abs(dot(a, b) - 1) < 1e-6
  private def angleDeg(a: V, b: V): Double =
    math.toDegrees(math.acos(math.max(-1.0, math.min(1.0, dot(a, b)))))

  /** The polygons of each cell of the alphabet used here, by their interior angles. */
  val polygons: Map[String, Set[Int]] = Map(
    "tet"      -> Set(3),
    "oct"      -> Set(3),
    "cube"     -> Set(4),
    "truncTet" -> Set(3, 6),
    "p3"       -> Set(3, 4),
    "p6"       -> Set(4, 6),
    "p8"       -> Set(4, 8),
    "p12"      -> Set(4, 12)
  )

  /** The polygon size whose interior angle is the given angle, if any. */
  def polygonOf(angle: Double): Option[Int] =
    Vector(3, 4, 6, 8, 12).find(n => math.abs(angle - (180.0 - 360.0 / n)) < 1e-4)

  /** One corner of a star: the cell's label and its unit edge directions from the vertex. */
  final case class Corner(cell: String, edges: Vector[V])

  /** A face at the vertex within a corner: two edge directions and the polygon size. */
  final case class Face(a: V, b: V, sides: Int):
    def normal: V = norm(cross(a, b))

  /** The faces of a corner: the edge pairs whose angle is an interior angle of one of the cell's polygons (an
    * octahedron's 90° diagonals are not faces, its polygons being triangles only).
    */
  def cornerFaces(c: Corner): Vector[Face] =
    val allowed = polygons.getOrElse(c.cell, Set.empty)
    for
      i <- c.edges.indices.toVector; j <- c.edges.indices if i < j
      n <- polygonOf(angleDeg(c.edges(i), c.edges(j))) if allowed(n)
    yield Face(c.edges(i), c.edges(j), n)

  /** Whether two faces at the vertex are the same geometric face: the same pair of edge directions. */
  def sameFace(g: Face, f: Face): Boolean =
    (sameDir(g.a, f.a) && sameDir(g.b, f.b)) || (sameDir(g.a, f.b) && sameDir(g.b, f.a))

  /** The geometric faces of a star at the vertex, each once, with the corners (cells) presenting it — two for
    * every face of a closed star.
    */
  def starFaces(star: Vector[Corner]): Vector[(Face, Vector[Corner])] =
    val acc = collection.mutable.ArrayBuffer.empty[(Face, Vector[Corner])]
    for c <- star; f <- cornerFaces(c) do
      acc.indexWhere((g, _) => sameFace(g, f)) match
        case -1 => acc += ((f, Vector(c)))
        case i  => acc(i) = (acc(i)._1, acc(i)._2 :+ c)
    acc.toVector

  /** The star of species i as corners. */
  def starOf(i: Int): Vector[Corner] =
    val st = SpeciesEnumerator.species(i).state
    st.corners.map(c => Corner(c.cell.label, c.vids.map(v => norm(st.posMid(v)))))

  /** The normals of the face planes through the vertex, one per line. */
  def candidatePlanes(star: Vector[Corner]): Vector[V] =
    star.flatMap(cornerFaces).map(_.normal).foldLeft(Vector.empty[V]) { (acc, n) =>
      if acc.exists(sameLine(_, n)) then acc else acc :+ n
    }

  /** A side of a junction plane: the cells by label, and the prisms among them by whether their axis (the
    * lateral edge, the one edge perpendicular to the other two) is the plane's normal.
    */
  final case class Side(cells: Map[String, Int], prismsAlong: Int, prismsAcross: Int)

  /** A junction plane: the polygon sizes of the faces in the plane in cyclic order around the vertex, the two
    * sides, and the faces of the star lying in the plane.
    */
  final case class Split(normal: V, pattern: Vector[Int], below: Side, above: Side, faces: Vector[Face])

  private def isPrism(cell: String) = cell.startsWith("p")

  /** The axis of a prism corner: its edge perpendicular to the other two. */
  def prismAxis(c: Corner): Option[V] =
    c.edges.indices.find(j =>
      c.edges.indices.filter(_ != j).forall(l => math.abs(dot(c.edges(j), c.edges(l))) < 1e-6)
    ).map(c.edges)

  /** The split of a star by the plane through the vertex with the given normal: None if some cell has edges
    * strictly on both sides, or if the faces lying in the plane do not close up to 360° around the vertex.
    */
  def planeSplit(star: Vector[Corner], n: V): Option[Split] =
    def side(v: V): Int = { val d = dot(v, n); if d > 1e-6 then 1 else if d < -1e-6 then -1 else 0 }
    // the faces lying in the plane, each geometric face once (it is a face of the cells on both sides)
    val inPlane         = star.flatMap(cornerFaces).filter(f => side(f.a) == 0 && side(f.b) == 0)
      .foldLeft(Vector.empty[Face])((acc, f) => if acc.exists(sameFace(_, f)) then acc else acc :+ f)
    val total           = inPlane.map(f => 180.0 - 360.0 / f.sides).sum
    // each cell's side: the sign of its edges off the plane, None if it has edges on both sides or none
    val sides           = star.map { c =>
      val s = c.edges.map(side).filter(_ != 0).distinct
      (c, if s.size == 1 then Some(s.head) else None)
    }
    if inPlane.isEmpty || math.abs(total - 360.0) > 1e-4 || sides.exists(_._2.isEmpty) then None
    else
      // the cyclic order of the in-plane faces around the vertex, by the angle of their bisectors
      val ref                    = inPlane.head.a
      val up                     = norm(cross(n, ref))
      def bisectorAngle(f: Face) =
        val m = norm((f.a._1 + f.b._1, f.a._2 + f.b._2, f.a._3 + f.b._3))
        math.atan2(dot(m, up), dot(m, ref))
      val pattern                = inPlane.sortBy(bisectorAngle).map(_.sides)
      def mk(sgn: Int): Side     =
        val cs              = sides.filter(_._2.contains(sgn)).map(_._1)
        val (along, across) =
          cs.filter(c => isPrism(c.cell)).partition(c => prismAxis(c).exists(sameLine(_, n)))
        Side(cs.groupBy(_.cell).view.mapValues(_.size).toMap, along.size, across.size)
      Some(Split(n, pattern, mk(-1), mk(1), inPlane))

  /** Every junction plane of species i. */
  def junctionPlanes(i: Int): Vector[Split] =
    val star = starOf(i)
    candidatePlanes(star).flatMap(planeSplit(star, _))

  /** The species whose label contains every one of the given cell labels. */
  def speciesWith(cells: String*): Vector[Int] =
    SpeciesEnumerator.species.indices.toVector.filter { i =>
      val lab = SpeciesCorona.label(i)
      cells.forall(c => lab.contains(s"$c:"))
    }

  /** A pattern as a cyclic word up to rotation and reversal, for comparisons. */
  def cyclic(p: Vector[Int]): Vector[Int] =
    val rots = for a <- Vector(p, p.reverse); r <- p.indices yield a.drop(r) ++ a.take(r)
    rots.min(using math.Ordering.Implicits.seqOrdering)
