# krotenheerdt-sequence — verification artifact

Machine-checked companion to the paper

> **The three-dimensional Krötenheerdt sequence and its vanishing point.**

A face-to-face honeycomb of Euclidean 3-space by unit-edge convex uniform polyhedra is k-uniform Krötenheerdt
when its vertices fall into exactly k orbits carrying k pairwise distinct vertex stars. The paper determines
the number N_k of such honeycombs for every k:

| k | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | ≥ 11 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| N_k | 28 | 57 | 119 | 146 | 122 | 78 | 18 | 2 | 0 | 0 | 0 |

The sequence vanishes from k = 9 on, one row after the planar Krötenheerdt sequence 11, 20, 39, 33, 15, 10, 7.
This repository is the machine-checked half of the proof: the structure theorems are proved in the paper, and
every finite fact they use, every count and every class is asserted here.

It contains the paper's **claim specs** and the modules that carry the paper's **own** results — the stacking
model of the prism world, the symmetries of a stacking word, the exact enumeration of the words with every cut
the paper proves, the star-table facts, the slab world and the hexagon world. Every shared engine — the cell alphabet, the
species table, the gluing atlases and shells, the fair k-sets, the k-patterns and the realization of a
symbol, the Delaney–Dress side of a vertex star and the planar SAT assembler — is the pinned
[`research-core`](https://github.com/scala-tessella/research-core) `0.13.1` library.

## Reproduce

```bash
sbt test                          # the fast tier: every fixture, filter and canary — a few minutes
sbt -Dcertificates test           # + the lemma certificates of the stacking words — minutes
sbt -Dplanar test                 # + the planar sequence T_3 .. T_7 by the SAT assembler
sbt -Dlifts.k2 test               # + the prismatic lifts, row by row (also -Dlifts.k3, -Dlifts.k4, -Dlifts.k5)
sbt -Dcensus.k2 test              # + the census rows (also -Dcensus.k3, -Dcensus.k4: up to about nine hours)
sbt -Denumerate=5,6 test          # + the enumeration rows (-Denumerate alone: 2 to 10, about fifteen hours)
sbt -Ddossier=5,6,7,8 test        # + the dossier of every class from k = 5 on (some tuples take hours)
sbt -Dworlds=4 test               # + the admissible sets by world (the fair-quadruple sweep, about 3 hours)
```

The fast tier needs no external tools and no fixtures beyond the known answers it checks against. The long
runs are opt-in, each asserting its result and writing its certificate under `certs/`. Parallel runs take
`-Dcensus.par=N` and `-Denumerate.threads=N`.

`certs/` is committed, so the certificates the paper cites are readable without running anything. They are an
**output** of this repository and never an input: no spec reads them back, so a stale or hand-edited
certificate cannot make anything pass. Caches (the connectivity tables) and working records of the census
drivers go under `target/`.

The census runs sweep every folding tuple: the connectivity gate, which skips tuples by a theorem the paper
does not state, is off unless `-Dcensus.gate=conn` turns it on.

## Claim → check

Run one with `sbt "testOnly *<SpecName>"`.

| Paper result | Spec | Certifies |
|---|---|---|
| The stacking model | `StripStackingSpec` | The build of a word and its minimal symbol, anchored on the classical prismatic honeycombs and a 120-chamber 4-uniform class; every word up to length 2 gives a valid symbol; the species of a junction read off a four-layer window. |
| The symmetries of a word | `StripSymmetrySpec` | The in-plane group, the image of a segment with the drift of the grid, the reversal of a word (a mirror image keeps the symbol), the junction maps and the canonical labels. |
| The lemmas as cuts, and the search | `StripEnumerationSpec` | Plain levels and the axis periods they admit, the glide cut, base rows and the single-period condition, the vertex count of a level, the prefix orbit constraints, the block condition, the period-4 runs cut, the seven-species cut; the vertex orbits of a word; the searches at k = 1 and, for the glide word, at k = 7 over period 4. |
| The lemma certificates | `StripCertificatesSpec` (`-Dcertificates`) | The star kinds at a base row's lower level (135 windows), the eleven species reachable at a junction (747 contexts, by sector of periods), the junction dictionary of period-2 words (103 windows), the levels of period-4 runs (447 contexts), the level dictionary, the k = 1 search over periods 2 and 3. |
| Facts F1–F3 | `StarPlanesSpec` | The species with a truncated tetrahedron and the octet species, the junction planes of the hexagon-world and octet stars with the type of each side, the two stars with a kagome plane and a K side. |
| Fact F0 | `CubicFamilyIsolationSpec` | Eight cubic-family species share no edge figure with another species, the cantellated-cubic and runcic-cubic stars only with each other; that pair is admissible at k = 2. |
| The two-direction claim, locally | `PrismAxesSpec` | Every one of the 34 stars carries at most two prism-axis directions, perpendicular when two. |
| The slab world | `SlabNecklacesSpec`, `SlabWorldK5Spec` | The necklaces 6, 20, 24, 16, 0; the six 1-uniform classes against the completeness audit's count; the Barlow stackings; the quasi-period bound with an over-sweep; nothing at k = 5. |
| The hexagon world | `HexagonNecklacesSpec` | The words over the two placements of the quarter-cubic slab and the kagome layer, the junction and apex species, the necklaces 3, 6, 4 at k = 2, 3, 4 with the species sets of the thirteen census classes, none beyond; Andreini's 13′ as the word S1 S0; an over-sweep past the quasi-period bound. |
| The census rows k = 2, 3, 4 | `SymbolK2CensusSpec`, `SymbolK3CensusSpec`, `SymbolK4CensusSpec` (`-Dcensus.k2` …) | Every admissible k-set swept over its folding tuples to the scope of 128 chambers, every minimal symbol realized; for k = 2, 3 every tail beyond the scope swept and empty, for k = 4 the band to 160 chambers (130 + 12 classes, 142 of the row's 146; the four of 168 chambers are found by the enumeration row); the canaries in the fast tier. |
| The realization of a symbol | `SymbolRealizationFilterSpec`, `KPatternsSpec` | The standalone realization filter on known-genuine patterns across the worlds; the k-role pattern machinery against the k = 2 case. |
| The prismatic lifts | `PrismaticLiftsK2Spec` … `PrismaticLiftsK5Spec`, `K3LiftIdentificationSpec` (`-Dlifts.k2` …) | Every planar Krötenheerdt tiling of rows 2 to 5 lifts to a unique species set, the lift keys distinct, every set admissible; the k = 3 lifts in their sets' census. |
| The planar sequence | `PlanarSequenceSpec` (`-Dplanar`) | T_n = 11, 20, 39, 33, 15, 10, 7 re-derived by the SAT assembler. |
| The enumeration rows | `EnumerationRowsSpec` (`-Denumerate`) | Each row returns exactly its known classes, the stackings together with the lifts the search reaches as words, and its two-direction words (rows of both axes) number W_k: 27 at k = 2 (18 + 9), 67 at k = 3 (50 + 17), 107 at k = 4 (93 + 14), 115 at k = 5 (107 + 8), 69 at k = 6 (68 + 1), 11 at k = 7, 2 at k = 8, none at k = 9 and 10. |
| The worlds table | `WorldsTableSpec` (`-Dworlds=4`) | The world of a k-set read off its species' cells; the admissible sets at k = 4 by world, 294, 3, 6, 36. |
| The dossiers | `DossierSpec` (`-Ddossier`) | Every class from k = 5 on valid and minimal with k distinct species, and the whole content of its folding tuple. |

## License

Apache License 2.0.
