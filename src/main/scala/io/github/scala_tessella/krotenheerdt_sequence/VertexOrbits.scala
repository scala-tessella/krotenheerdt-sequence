package io.github.scala_tessella.krotenheerdt_sequence

import StripStacking.*

/** The vertex orbits of a stacking word's honeycomb. The minimal symbol is reached by successive congruence
  * quotients of the translation quotient; composing the chamber maps of those quotients carries every
  * chamber's vertex, reduced modulo the lattice, to its orbit in the minimal symbol. A k-uniform Krötenheerdt
  * word has k orbits, each carrying one species, the k species pairwise distinct.
  */
object VertexOrbits:

  /** The map reduced vertex → orbit and the species of each orbit; None if the word has no symbol. */
  def of(layers: Vector[Layer]): Option[(Map[V, Int], Vector[Set[Int]])] =
    if !consistent(layers) then None
    else
      val (cells, per, (px, py)) = buildLayers(layers)
      val tx                     = layers.collect { case Layer.Tri(false, hx, p) if hx.nonEmpty => p }.foldLeft(1)(lcm)
      val ty                     = layers.collect { case Layer.Tri(true, hx, p) if hx.nonEmpty => p }.foldLeft(1)(lcm)
      flags(cells, per, Vector((2 * tx, 0, 0, 0), (0, 2 * ty, 0, 0), layersVector(layers)), px, py).map {
        fl =>
          def mapOf(cls: Vector[Int]): Vector[Int] =
            val newId = cls.distinct.zipWithIndex.toMap
            cls.map(newId)
          var cur                                  = quotient(S(fl.s, fl.m01, fl.m23, fl.cell), fl.classes)
          var map                                  = mapOf(fl.classes)
          var go                                   = true
          while go do
            congruence(cur) match
              case Some(cls) =>
                val m = mapOf(cls)
                map = map.map(m)
                cur = quotient(cur, cls)
              case None      => go = false
          val vo                                   = cur.orbitsOf(Vector(1, 2, 3))
          val byVertex                             = fl.vert.indices.map(ch => reduce(fl.vert(ch), per, px, py) -> vo(map(ch))).toMap
          (byVertex, speciesOf(cur))
      }

  /** Whether a word is Krötenheerdt at its own k: every orbit one species, the species pairwise distinct. */
  def krotenheerdt(layers: Vector[Layer]): Option[Int] =
    of(layers).map(_._2).filter(sp => sp.forall(_.size == 1) && sp.flatten.toSet.size == sp.size).map(_.size)
