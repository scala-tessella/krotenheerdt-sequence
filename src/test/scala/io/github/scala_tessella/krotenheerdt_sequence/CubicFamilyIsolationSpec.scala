package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.research_core.{KSetShell, SpeciesCorona, SpeciesEnumerator}
import io.github.scala_tessella.research_core.MonoShell

/** Fact F0 of the Krötenheerdt sequence paper, as it holds: the species with a cubic-family cell
  * (cuboctahedron, rhombicuboctahedron, truncated cuboctahedron, truncated octahedron, truncated cube) lie in
  * no admissible k-set for k ≥ 3, and at k = 2 in one only, which carries no honeycomb.
  *
  * Two species of one Krötenheerdt honeycomb joined by an edge share that edge's figure, so an admissible set
  * is connected in the shared-figure graph (the substrate of `KSetShell`). Eight of the ten cubic-family
  * species share a figure with no other species; the other two — the cantellated-cubic and the runcic-cubic
  * stars — share figures only with each other. So no admissible set with k ≥ 3 contains a cubic-family
  * species, and at k = 2 only that pair can; it is admissible (the edge relation holds both ways), and the k =
  * 2 census finds no symbol on it (`SymbolK2CensusSpec`).
  */
class CubicFamilyIsolationSpec extends AnyFlatSpec with Matchers:

  private val labels: Vector[String] = SpeciesEnumerator.species.indices.toVector.map(SpeciesCorona.label)
  private def cubicFamily(l: String) =
    Vector("co:", "rco:", "tco:", "truncOct:", "truncCube:").exists(l.contains)
  private val cubic                  = labels.indices.filter(i => cubicFamily(labels(i))).toVector
  private val rcoPair                = Set("{cube:2 co:1 rco:2}#1", "{tet:1 cube:1 rco:3}#1")

  /** The component of a species in the shared-figure graph. */
  private def component(s: Int): Set[Int] =
    val adj                                                 = SpeciesCorona.analysis.adjacency
    @annotation.tailrec
    def grow(seen: Set[Int], frontier: List[Int]): Set[Int] = frontier match
      case Nil     => seen
      case x :: xs =>
        val next = adj.getOrElse(x, Vector.empty).filterNot(seen).distinct.toList
        grow(seen ++ next, next ++ xs)
    grow(Set(s), List(s))

  "the cubic-family species" should "be ten" in {
    cubic.size shouldBe 10
  }

  "F0" should "leave eight cubic-family species alone and the rco pair together in the shared-figure graph" in {
    val components = cubic.map(s => labels(s) -> component(s).map(labels)).toMap
    val alone      = components.filter(_._2.size == 1).keySet
    alone.size shouldBe 8
    for l <- rcoPair do components(l) shouldBe rcoPair
    // hence every connected set of three or more species avoids the cubic family
    components.values.map(_.size).max shouldBe 2
  }

  it should "find the rco pair admissible at k = 2, with the edge relation both ways" in {
    val Vector(a, b) = rcoPair.toVector.map(labels.indexOf).sorted
    val flags        = MonoShell.Flags()
    KSetShell.shellWithTarget(a, b, Vector(a, b), flags).sat shouldBe true
    KSetShell.shellWithTarget(b, a, Vector(a, b), flags).sat shouldBe true
    KSetShell.fairKSet(Vector(a, b), flags).fair shouldBe true
  }
