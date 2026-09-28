package io.github.scala_tessella.krotenheerdt_sequence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StripStacking.*
import io.github.scala_tessella.research_core.{SpeciesCorona, SymbolCatalog, StarFoldings}
import io.github.scala_tessella.research_core.StarFoldings.{fold, symmetryOf}
import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionOf}
import java.nio.file.{Files, Path}

/** THE DOSSIERS of the classes the enumeration finds from k = 5 on: every class of a row is VALID and MINIMAL
  * with k pairwise distinct species; its FOLDING TUPLE — per orbit the species and the subgroup of its star
  * that the orbit folds — is read off the symbol; and the census enumerator, run uncapped at that tuple,
  * finds exactly the known classes of the tuple and no other minimal symbol: the stacking search and the
  * census agree on the complete content of every tuple they share. Writes `certs/dossiers-k<k>.txt`.
  *
  * Opt-in: `sbt -Ddossier=7 test` (or `-Ddossier=5,6,7,8`). The σ₀ enumeration grows with the tuple: minutes
  * for most tuples, some hours at 336 and 384 chambers; `-Ddossier.max=<chambers>` leaves out the larger
  * tuples.
  */
class DossierSpec extends AnyFlatSpec with Matchers:

  private def say(s: String): Unit = synchronized { println(s); System.out.flush() }

  /** The folding tuple of a symbol: per orbit, in species order, the species and the folded subgroup. */
  private def tupleOf(x: S): Option[(Vector[Int], Vector[Set[StarFoldings.Perm]])] =
    val k     = x.orbitsOf(Vector(1, 2, 3)).max + 1
    val parts = (0 until k).toVector.map(o => foldedStarSubs.getOrElse(orbitStarKey(x, o), Vector.empty))
    Option.when(parts.forall(_.nonEmpty)) {
      val chosen = parts.map(_.head).sortBy(_._1)
      (chosen.map(_._1), chosen.map(_._2))
    }

  "every class from k = 5 on" should "be valid, minimal and the whole content of its tuple (-Ddossier)" in:
    assume(OptIn.enabled("dossier"))
    val rows = sys.props("dossier").split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector match
      case Vector() => (5 to 8).toVector
      case chosen   => chosen
    val max  = sys.props.get("dossier.max").map(_.toInt).getOrElse(Int.MaxValue)
    for k <- rows do
      val cert    = StringBuilder(s"THE DOSSIERS OF ROW k = $k\n\n")
      val classes = KnownRows.row(k).filter(_.chambers <= max).map { r =>
        val x = symbolOfLayers(r.layers).get
        val s = x.toSym
        withClue(s"k = $k, ${r.tag}: "):
          KnownRows.keyTag(canonicalKey(x)) shouldBe r.tag
          SymbolCatalog.valid(s) shouldBe true
          SymbolCatalog.isMinimal(s) shouldBe true
          KnownRows.classOf(k, r.layers).isDefined shouldBe true // k orbits, k distinct species
        (r, x, tupleOf(x).getOrElse(fail(s"${r.tag}: an orbit is not a folded star")))
      }
      for ((sps, subs), members) <- classes.groupBy(_._3).toVector.sortBy(_._2.head._1.chambers) do
        val t0             = System.currentTimeMillis
        val u              = unionOf(sps.indices.toVector.map(i => fold(symmetryOf(sps(i)), subs(i))))
        val (sols, capped) = enumerateSigma0(u, cap = 1000000)
        val keys           = sols.flatMap { s0 =>
          val ss = SymbolCatalog.symOf(u, sps, s0)
          Option.when(SymbolCatalog.isMinimal(ss))(
            KnownRows.keyTag(canonicalKey(S(Vector(ss.s0, ss.s1, ss.s2, ss.s3), ss.m01, ss.m23, ss.cell)))
          )
        }.toSet
        val line           =
          s"tuple ${sps.map(SpeciesCorona.label).mkString(" ~ ")} |H| = ${subs.map(_.size).mkString(",")}, " +
            s"${u.size} chambers: ${sols.size} sigma0, capped $capped, ${keys.size} minimal keys; " +
            s"known ${members.map(_._1.tag).sorted.mkString(" ")} [${(System.currentTimeMillis - t0) / 1000}s]"
        say(s"k = $k $line")
        cert ++= line + "\n"
        withClue(s"k = $k, $line: "):
          capped shouldBe false
          keys shouldBe members.map(_._1.tag).toSet
      Files.createDirectories(Path.of("certs"))
      Files.writeString(Path.of("certs", s"dossiers-k$k.txt"), cert.toString): Unit
