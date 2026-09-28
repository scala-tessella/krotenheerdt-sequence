// krotenheerdt-sequence — the machine-checked verification artifact for the paper
//   "The three-dimensional Krötenheerdt sequence and its vanishing point".
// It contains the paper's claim specs and the modules that carry its own results — the stacking model of the
// prism world, the symmetries of a stacking word, the exact enumeration of the words with every cut the paper
// proves, the star-table facts and the slab world. Every shared engine — the cell alphabet, the species table,
// the gluing atlases and shells, the k-patterns and the realization of a symbol, and the Delaney-Dress side of
// a vertex star — is the pinned research-core library. The specs live in package
// io.github.scala_tessella.krotenheerdt_sequence and import the library from io.github.scala_tessella.research_core.
//
// `sbt test` runs the fast tier in a few minutes. The long runs — the lemma certificates, the census rows and
// the enumeration rows — are opt-in through system properties named in the README, each asserting its result
// and writing its certificate under certs/.

ThisBuild / scalaVersion  := "3.9.0"
ThisBuild / organization  := "io.github.scala-tessella"
ThisBuild / versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .settings(
    name           := "krotenheerdt-sequence",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "io.github.scala-tessella" %% "research-core"        % "0.13.1",
      "io.github.scala-tessella" %% "research-core-solver" % "0.13.1",
      "org.scalatest"            %% "scalatest"     % "3.2.20" % Test
    ),
    // the parallel searches and the certificate runs are memory-hungry; the opt-in runs are selected by system
    // properties, which a forked test JVM would not inherit — so the tests run in sbt's JVM, not forked
    Test / parallelExecution := false
  )
