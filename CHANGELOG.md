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
