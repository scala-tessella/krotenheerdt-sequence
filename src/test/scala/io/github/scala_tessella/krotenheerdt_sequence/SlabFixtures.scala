package io.github.scala_tessella.krotenheerdt_sequence

import BarlowFixtures.V3
import SlabNecklaces.junctionOf
import io.github.scala_tessella.research_core.SpeciesEnumerator.species

/** Independent KNOWN-ANSWER fixtures for the the slab world slab pairs (the fixture-first discipline,
  * extending `BarlowFixtures` to the full slab alphabet): the certified k = 2 slab honeycombs are built
  * DIRECTLY from their `SlabNecklaces` words — vertical step words over {+, −, 0} as junction-plane lattice
  * clouds (octet slabs step the ℤ/3 position and have height √(2/3), prism slabs keep it and have height 1),
  * horizontal junction words over {s, g} as square-lattice interface clouds with the half-unit offsets and
  * axis flips of the slab world's layer walk — and germs are extracted geometrically by
  * `BarlowFixtures.germDomainsOf`. No pattern-search machinery is used in the construction, so the fixtures
  * adjudicate the k = 2 symbol checks exactly as the Barlow fixtures do.
  */
object SlabFixtures:

  private def bySupport(sup: String): Vector[Int] =
    species.indices.toVector.filter(i => species(i).showSupport == sup)

  /** The five slab species as the species table indices, keyed by their the slab world letter
    * (SlabNecklaces.Species labels).
    */
  lazy val speciesByLetter: Map[Char, Int] =
    val Vector(h, c) = bySupport("{tet:8 oct:6}").sortBy(i => species(i).figures.size).reverse
    val e            = bySupport("{tet:4 oct:3 p3:6}").head
    val prisms       = bySupport("{p3:12}")
    val x            = prisms.find(i => species(i).figures.exists(_._1.size == 5)).get
    val p            = prisms.find(_ != x).get
    Map('c' -> c, 'h' -> h, 'e' -> e, 'p' -> p, 'x' -> x)

  private val eta   = math.sqrt(6.0) / 3.0        // octet slab height
  private val hh    = math.sqrt(3.0) / 2.0        // strip layer thickness
  private val delta = (0.5, math.sqrt(3.0) / 6.0) // in-plane shift per ℤ/3 position step

  /** All junction vertices of a vertical step word with species letters, junction planes -big..big, lattice
    * coordinates -m..m. Plane 0 at z = 0, position 0; slab j (plane j → j+1) has step word(j mod P).
    */
  def verticalVertices(word: Vector[Int], big: Int, m: Int): Vector[(V3, Char)] =
    val P              = word.size
    def step(j: Int)   = word(((j % P) + P) % P)
    def height(j: Int) = if step(j) == 0 then 1.0 else eta
    val z              = collection.mutable.Map(0 -> 0.0)
    val p              = collection.mutable.Map(0 -> 0)
    for j <- 0 until big do
      z(j + 1) = z(j) + height(j)
      p(j + 1) = p(j) + step(j)
    for j <- 0 until big do
      z(-j - 1) = z(-j) - height(-j - 1)
      p(-j - 1) = p(-j) - step(-j - 1)
    (for
      j <- (-big to big).toVector
      a <- -m to m
      b <- -m to m
    yield
      val q = ((p(j) % 3) + 3) % 3
      val x = a + 0.5 * b + q * delta._1
      val y = b * math.sqrt(3.0) / 2 + q * delta._2
      ((x, y, z(j)), junctionOf(step(j - 1), step(j)).label.head)
    ).toVector

  /** All interface vertices of a horizontal junction word with species letters: interface j at z = j·√3/2
    * carries a unit square lattice whose offset advances by half a unit along the lower layer's perpendicular
    * axis, the axis flipping exactly at g-junctions (letter 1) — the layer walk of the slab world, vertices
    * only. Letters read the word directly: 0 = s = p, 1 = g = x.
    */
  def horizontalVertices(word: Vector[Int], big: Int, m: Int): Vector[(V3, Char)] =
    val P                                          = word.size
    def md(a: Int)                                 = ((a % P) + P) % P
    def flip(letter: Int, ax: Boolean)             = if letter == 1 then !ax else ax
    def topOff(ax: Boolean, off: (Double, Double)) =
      if ax then (off._1, off._2 + 0.5) else (off._1 + 0.5, off._2)
    def invTop(ax: Boolean, off: (Double, Double)) =
      if ax then (off._1, off._2 - 0.5) else (off._1 - 0.5, off._2)
    // layer j sits between interfaces j and j+1; off(j)/ax(j) are its base-interface offset and its axis
    val off                                        = collection.mutable.Map(0 -> (0.0, 0.0))
    val ax                                         = collection.mutable.Map(0 -> true)
    for j <- 0 until big do
      off(j + 1) = topOff(ax(j), off(j))
      ax(j + 1) = flip(word(md(j + 1)), ax(j))
    for j <- 0 until big do
      ax(-j - 1) = flip(word(md(-j)), ax(-j))
      off(-j - 1) = invTop(ax(-j - 1), off(-j))
    (for
      j <- (-big to big).toVector
      a <- -m to m
      b <- -m to m
    yield ((a + off(j)._1, b + off(j)._2, j * hh), if word(md(j)) == 0 then 'p' else 'x')).toVector

  /** The labeled vertex cloud of a k = 2 slab necklace class, sized for germ extraction AND for the
    * development-matching check (base vertices sit within ~1.6 of the cloud center, so the in-plane margin
    * comfortably exceeds the 3.05 matching radius).
    */
  def cloudOf(nc: SlabNecklaces.NecklaceClass): Vector[(V3, Char)] =
    if nc.vertical then verticalVertices(nc.word, 8, 6) else horizontalVertices(nc.word, 6, 6)
