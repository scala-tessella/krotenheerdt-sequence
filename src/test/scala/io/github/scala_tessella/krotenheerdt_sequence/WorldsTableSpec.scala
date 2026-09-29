package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.HoneycombAlphabet.CellType
import io.github.scala_tessella.research_core.{KSetShell, MonoShell, SpeciesEnumerator}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE WORLDS TABLE: the admissible k-sets sorted by the cells of their species (the paper's `tab:worlds`). A
  * k-set lies in the hexagon world when a species has a truncated tetrahedron, in the mixed sets when an
  * octet species (tetrahedra or octahedra) meets a cube or hexagonal-prism species, in the slab world when
  * every cell is a tetrahedron, an octahedron or a triangular prism, in the prism world when every cell is a
  * cube or a prism; a cubic-family species puts it in none (F0: no such set from k = 3 on).
  *
  * The classification is fast; the sets are the admissible ones of `KSetShell.fairKSets`, opt-in:
  * `-Dworlds=4` (the fair-quadruple sweep, about 2 h 45 min on one thread — the same sweep the k = 4 census
  * battery runs), `-Dworlds=5`, `-Dworlds=6` (the sweeps behind the paper's rows 5 and 6, cost not yet
  * measured).
  */
class WorldsTableSpec extends AnyFlatSpec with Matchers:

  enum World:
    case Prism, Slab, Hexagon, Mixed, CubicFamily

  private val cubicFamily: Set[CellType] = Set(
    CellType.Cuboctahedron,
    CellType.TruncOct,
    CellType.TruncCube,
    CellType.Rhombicuboctahedron,
    CellType.TruncCuboctahedron
  )
  private val octetCells: Set[CellType]  = Set(CellType.Tet, CellType.Oct)
  private val slabCells: Set[CellType]   = Set(CellType.Tet, CellType.Oct, CellType.P3)
  private val prismCells: Set[CellType]  =
    Set(CellType.Cube, CellType.P3, CellType.P6, CellType.P8, CellType.P12)

  private def cells(s: Int): Set[CellType] = SpeciesEnumerator.species(s).counts.keySet

  def worldOf(kSet: Vector[Int]): World =
    val cs = kSet.map(cells)
    if cs.exists(_.exists(cubicFamily)) then World.CubicFamily
    else if cs.exists(_.contains(CellType.TruncTet)) then World.Hexagon
    else if cs.exists(_.exists(octetCells)) &&
      cs.exists(c => c.contains(CellType.Cube) || c.contains(CellType.P6))
    then World.Mixed
    else if cs.forall(_.subsetOf(slabCells)) then World.Slab
    else if cs.forall(_.subsetOf(prismCells)) then World.Prism
    else fail(s"k-set ${kSet.mkString(",")} fits no world")

  "the world of a k-set" should "be read off the cells of its species" in:
    // fixtures: the slab quadruple of the octet stars, the elongated star and the parallel prism star
    worldOf(Vector(30, 31, 32, 33)) shouldBe World.Slab
    // the hexagon-world stars 22 (S|K), 23 and 24 (S|S) with the kagome lift 21
    worldOf(Vector(21, 22, 23, 24)) shouldBe World.Hexagon
    // a prism-world pair: the kagome lift and the parallel prism star
    worldOf(Vector(21, 30)) shouldBe World.Prism

  /** The paper's rows: admissible sets by world, k = 4, 5, 6. */
  private val expected: Map[Int, Map[World, Int]] = Map(
    4 -> Map(World.Prism -> 294, World.Slab -> 3, World.Hexagon -> 6, World.Mixed -> 36),
    5 -> Map(World.Prism -> 811, World.Slab -> 1, World.Hexagon -> 28, World.Mixed -> 161),
    6 -> Map(World.Prism -> 1898, World.Hexagon -> 119, World.Mixed -> 573)
  )

  "the admissible k-sets" should "fall into the worlds as the paper's table says (-Dworlds)" in:
    assume(OptIn.enabled("worlds"))
    val rows = sys.props("worlds").split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector
    for k <- rows do
      val flags = MonoShell.Flags()
      val sets  = KSetShell.fairKSets(k, flags).filter(_.fair).map(_.kSet)
      val count = sets.groupMapReduce(worldOf)(_ => 1)(_ + _)
      withClue(s"k = $k: ")(count shouldBe expected(k))
      flags.items.distinct shouldBe empty
