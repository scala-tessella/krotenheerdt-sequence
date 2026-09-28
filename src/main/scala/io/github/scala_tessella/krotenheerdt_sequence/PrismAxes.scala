package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesEnumerator
import io.github.scala_tessella.research_core.HoneycombAlphabet.CellType

/** PRISM AXES per species star (the local form of the paper's two-direction theorem): the axis directions of
  * the prism cells (P3, P6, P8, P12 — the lateral edge at a corner is the one neighbour perpendicular to the
  * other two), clustered up to sign, with the cube edge directions apart. Every star carries at most two
  * prism-axis directions, perpendicular when two (`PrismAxesSpec`).
  */
object PrismAxes:

  type V = (Double, Double, Double)
  def norm(a: V): V             =
    val n = math.sqrt(a._1 * a._1 + a._2 * a._2 + a._3 * a._3)
    val u = (a._1 / n, a._2 / n, a._3 / n)
    val s =
      if math.abs(u._1) > 1e-9 then math.signum(u._1)
      else if math.abs(u._2) > 1e-9 then math.signum(u._2)
      else math.signum(u._3)
    (u._1 * s, u._2 * s, u._3 * s)
  def dot(a: V, b: V): Double   = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  def same(a: V, b: V): Boolean = math.abs(math.abs(dot(a, b)) - 1) < 1e-6
  def angle(a: V, b: V): Double = math.toDegrees(math.acos(math.min(1.0, math.abs(dot(a, b)))))

  val prisms: Set[CellType] = Set(CellType.P3, CellType.P6, CellType.P8, CellType.P12)

  /** One species' table: the prism-axis directions with the cells realizing each, and the cube edge
    * directions.
    */
  final case class Table(axes: Vector[(V, Map[String, Int])], cubeDirs: Vector[V]):
    def angles: Vector[Double] =
      for x <- axes.indices.toVector; y <- axes.indices if x < y yield angle(axes(x)._1, axes(y)._1)

  def tableOf(i: Int): Table =
    val st       = SpeciesEnumerator.species(i).state
    val axes     = collection.mutable.ArrayBuffer.empty[(V, Map[String, Int])]
    val cubes    = collection.mutable.ArrayBuffer.empty[V]
    for c <- st.corners do
      val dirs = c.vids.map(v => norm(st.posMid(v)))
      if prisms(c.cell) then
        require(dirs.size == 3, s"prism corner with ${dirs.size} neighbours")
        val lateral = dirs.indices
          .find(j => dirs.indices.filter(_ != j).forall(l => math.abs(dot(dirs(j), dirs(l))) < 1e-6))
          .getOrElse(throw new IllegalStateException(
            s"species $i: no lateral edge at a ${c.cell.label} corner"
          ))
        val a       = dirs(lateral)
        axes.indexWhere((r, _) => same(r, a)) match
          case -1 => axes += ((a, Map(c.cell.label -> 1)))
          case ix =>
            val (r, m) = axes(ix)
            axes(ix) = (r, m.updated(c.cell.label, m.getOrElse(c.cell.label, 0) + 1))
      else if c.cell == CellType.Cube then cubes ++= dirs
    val cubeDirs =
      cubes.foldLeft(Vector.empty[V])((acc, d) => if acc.exists(same(_, d)) then acc else acc :+ d)
    Table(axes.toVector, cubeDirs)
