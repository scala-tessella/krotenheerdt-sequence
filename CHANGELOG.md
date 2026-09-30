# Changelog

All notable changes to this verification artifact are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). Because the artifact backs
a paper, entries state what a re-check would find different from the previous release — a referee who checked
an earlier version should be able to tell from here whether the claims, the specs, or only the packaging moved.

## [Unreleased]

The complete verification surface for the paper, pinned to `research-core 0.13.1`: the stacking model, the
symmetries and the enumeration of the words with every cut; the star-table facts F0–F3 and the slab world;
the census rows k = 2, 3, 4 with their drivers; the prismatic lifts and the planar sequence re-derived by the
SAT assembler; the enumeration rows k = 4 to 10 against their known classes and the dossier of every class
from k = 5 on. The fast tier runs under `sbt test`; the long runs are opt-in and write `certs/`.

### Added

- `HexagonNecklaces` and `HexagonNecklacesSpec`: the hexagon-world necklaces, words over the quarter-cubic
  slab in its two placements and the kagome prism layer, 3, 6, 4 at k = 2, 3, 4 and none beyond, matched to
  the thirteen hexagon-world classes of the census.
- `WorldsTableSpec`: the admissible sets sorted by world, the k = 4 row opt-in (`-Dworlds=4`).
- The enumeration rows k = 2 and 3, and the count of two-direction words W_k asserted and written in every row's
  certificate.

### Changed

- The enumeration rows run their chains to closure: a chain closes when the label segment returns, within
  24 segments by the level-orbit lemma (the stack period within four, the layer word read on the drifting
  grid within 24, 16 without a period 3), so the cap on a word is 48k layers, where it was 8k. A re-check
  finds the same rows; the certificates record the new cap.
- `StarPlanesSpec` certifies F2 as the paper now states it: per star every junction plane with the faces of
  the star it contains (`StarPlanes.Split.faces`, `starFaces`), the face-sharing S|S star told from the
  other by its two tetrahedra sharing a face, the close-packing octet star from the cuboctahedral one by its
  shared triangles; the first-plane argument of the hexagon-world and separation theorems rests on it.
- `-Denumerate` alone runs the rows 2 to 10.
- The k = 4 census battery: the band pair on {cube:4 p3:2 p6:2}#1 ~ {cube:8}#1 ~ {p3:4 p6:4}#1 ~ {p3:4 p6:4}#3
  is realized by the filter of `research-core 0.13.1`, so all twelve band symbols are pinned realized; every
  set is checked before the battery reports, so one stale pin cannot hide the others. The battery writes
  its certificate, `certs/census-k4.txt`: every set's classes against its pin, its band and its capping,
  written before anything is asserted.
- The certificates of the chain: `enumeration-k2` to `enumeration-k10`, `dossiers-k5` to `dossiers-k8`, the
  lifts, the planar sequence and the census rows, regenerated under the cuts of the paper.
