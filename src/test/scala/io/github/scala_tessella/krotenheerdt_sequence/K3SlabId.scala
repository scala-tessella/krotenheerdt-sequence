package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SymbolCatalog.Sym

/** Shared k = 3 slab-class identification — a thin facade over the k-generic `KSlabId` (identical behavior;
  * the historical call sites keep their names): the certified derivation of a SlabNecklaces k = 3 class's
  * minimal three-species symbol, used by the k = 3 census.
  */
object K3SlabId:

  def k3Classes: Vector[SlabNecklaces.NecklaceClass] = KSlabId.classesOf(3)

  /** The species triple of a class, as sorted letters and species indices (the ctx role order). */
  def tripleOf(nc: SlabNecklaces.NecklaceClass): (Vector[Char], Vector[Int]) = KSlabId.kSetOf(nc)

  /** The certified minimal symbol of one k = 3 class; throws on any verification failure. */
  def deriveClass(nc: SlabNecklaces.NecklaceClass): Sym = KSlabId.deriveClass(nc)
