package io.github.scala_tessella.krotenheerdt_sequence

import io.github.scala_tessella.research_core.SpeciesCorona

import io.github.scala_tessella.research_core.StarFoldings

import io.github.scala_tessella.research_core.Sigma0Assembly.{enumerateSigma0, unionOf, ChamberUnion}
import io.github.scala_tessella.research_core.StarFoldings.{fold, subgroupsOfSpecies, symmetryOf}

/** THE ADJACENCY-AWARE EMPTINESS TEST. The desert is adjacency-level: parity (per-decoration-class evenness)
  * explains almost none of it. This tests criteria built from the σ₀ propagation structure itself: an
  * assignment forces its whole ⟨σ₂,σ₃⟩-orbit (Sigma0Assembly.assign), so any valid σ₀ restricted to such an
  * orbit is a decoration-preserving INTERTWINER onto another orbit (equivariant for σ₂ and σ₃), and the union
  * is connected only if the chosen cross-part orbit pairings span all k parts. Necessary conditions,
  * increasing strength:
  *
  *   - parity: every (m01, m23, cell) class has even cardinality (the σ₀-fixed-point caveat makes this the
  *     weak baseline; kept for comparison);
  *   - conn: the graph on the k parts with an edge where SOME cross-part orbit intertwiner exists must be
  *     connected;
  *   - span: a spanning tree of the parts must be realizable by pairwise orbit-DISJOINT intertwined orbit
  *     pairs (each σ₀ cross-pairing consumes both orbits whole, so distinct tree edges need distinct orbits).
  *
  * The face-closure tier (aimed at the conn-OK residue): a pairing of orbits is only ADMISSIBLE when some
  * intertwiner passes the PAIR-LOCAL face closure — (σ₀σ₁)-walks staying inside the matched pair are exact,
  * so they must close at lengths dividing m₀₁ (odd m₀₁ kills the identity self-pairing wherever σ₁ stays
  * inside the orbit — the all-triangle worlds). Criteria: fneed (every orbit has an admissible option), fspan
  * (span over admissible cross pairs), fmatch (orbits with no admissible self-pairing matched disjointly AND
  * the choice still spans the parts; node-capped, cap = conservative pass).
  *
  * Ground truth is the raw DFS (enumerateSigma0, cap 1 — autos-free, authoritative for emptiness). Any
  * criterion-BAD + DFS-solvable tuple is a SOUNDNESS VIOLATION and is reported loudly: it falsifies the
  * criterion (or its implementation), which is exactly the measurement wanted.
  *
  * Stratum mode args: pairs of "i:j:..." "a:b:..." (species; subgroup orders). Set mode: `set <lo> <sps>...`
  * — every residue tuple of each set (the bandTuples stream), DFS verification capped at -Dorbit.dfsmax
  * (default 200). Run: sbt "Test/runMain io.github.scala_tessella.krotenheerdt_sequence.Sigma0Orbits
  * 16:21:27:30 1:2:1:2"
  */
object Sigma0Orbits:

  export ChamberRings.{orbitsOf, intertwinersOf, intertwines}

  /** Pair-local face closure: with σ₀ pinned to `p` on its domain (one matched orbit pair), every (σ₀σ₁)-walk
    * staying inside the domain is exact, so it must close at a length dividing m₀₁ and may not exceed m₀₁
    * open; a walk that leaves the domain is locally undecidable and imposes nothing. Necessary: a valid σ₀
    * restricted to any of its matched orbit pairs is such a `p`.
    */
  def faceLocalOk(u: ChamberUnion, p: Map[Int, Int]): Boolean =
    p.keys.forall { c =>
      var a       = c
      var steps   = 0
      var verdict = true
      var decided = false
      while !decided do
        p.get(a) match
          case None    => decided = true // the walk left the pair: no local constraint
          case Some(b) =>
            a = u.s1(b)
            steps += 1
            if a == c then
              verdict = u.m01(c) % steps == 0
              decided = true
            else if steps > u.m01(c) then
              verdict = false
              decided = true
      verdict
    }

  /** Whether some involutive equivariant self-pairing of the orbit (σ₀ = id included — fixed points are
    * allowed) passes the pair-local face closure. Odd m₀₁ with σ₁-internal edges is the typical killer of the
    * identity option (a 2-step face walk cannot divide a triangle).
    */
  def selfFeasible(u: ChamberUnion, o: Vector[Int]): Boolean =
    intertwinersOf(u, o, o).exists(phi => phi.forall((x, y) => phi(y) == x) && faceLocalOk(u, phi))

  /** Whether some intertwiner oA → oB passes the pair-local face closure on the doubled map. */
  def crossFeasible(u: ChamberUnion, oA: Vector[Int], oB: Vector[Int]): Boolean =
    intertwinersOf(u, oA, oB).exists(phi => faceLocalOk(u, phi ++ phi.map((x, y) => (y, x))))

  /** Arc-consistency face tier: the admissible pairings of every orbit (involutive self-pairings and
    * any-orbit cross intertwiners, each pair-local face-feasible) are pruned to a fixpoint — a pairing
    * survives only while every σ₁-adjacent orbit still offers a COMPATIBLE pairing, where two pairings are
    * incompatible when they share an orbit (σ₀ pairs each orbit once) or when a (σ₀σ₁)-walk inside their
    * combined domain closes at a length not dividing m₀₁ (or overruns it). An emptied domain refutes the
    * tuple. Sound: a valid σ₀'s induced pairings are pairwise compatible, so each survives every revision.
    */
  def fpropOk(u: ChamberUnion, orbits: Vector[Vector[Int]]): Boolean =
    val orbAt = Array.fill(u.size)(-1)
    for (o, i) <- orbits.zipWithIndex; c <- o do orbAt(c) = i

    final case class Pairing(a: Int, b: Int, m: Map[Int, Int])
    val pairings = collection.mutable.ArrayBuffer.empty[Pairing]
    for i <- orbits.indices do
      for
        phi <- intertwinersOf(u, orbits(i), orbits(i))
        if phi.forall((x, y) => phi(y) == x) && faceLocalOk(u, phi)
      do pairings += Pairing(i, i, phi)
    for i <- orbits.indices; j <- orbits.indices if i < j do
      for phi <- intertwinersOf(u, orbits(i), orbits(j)) do
        val m = phi ++ phi.map((x, y) => (y, x))
        if faceLocalOk(u, m) then pairings += Pairing(i, j, m)

    val dom = Array.tabulate(orbits.size) { i =>
      pairings.zipWithIndex.collect { case (p, id) if p.a == i || p.b == i => id }.toSet
    }
    if dom.exists(_.isEmpty) then return false

    val adj                                 = Vector.tabulate(orbits.size)(i => orbits(i).map(c => orbAt(u.s1(c))).toSet - i)
    val cache                               = collection.mutable.Map.empty[(Int, Int), Boolean]
    def compat(pid: Int, qid: Int): Boolean =
      if pid == qid then true
      else
        val key = (math.min(pid, qid), math.max(pid, qid))
        cache.getOrElseUpdate(
          key, {
            val (p, q) = (pairings(pid), pairings(qid))
            if p.a == q.a || p.a == q.b || p.b == q.a || p.b == q.b then false
            else faceLocalOk(u, p.m ++ q.m)
          }
        )

    val queue = collection.mutable.Queue.from(for i <- orbits.indices; j <- adj(i) yield (i, j))
    while queue.nonEmpty do
      val (i, j) = queue.dequeue()
      val before = dom(i).size
      dom(i) = dom(i).filter(pid => dom(j).exists(qid => compat(pid, qid)))
      if dom(i).isEmpty then return false
      if dom(i).size != before then for k <- adj(i) if k != j do queue.enqueue((k, i))
    true

  /** Criteria verdicts for one folded union. */
  final case class Verdicts(
      parity: Boolean,
      conn: Boolean,
      span: Boolean,
      fneed: Boolean,
      fspan: Boolean,
      fmatch: Boolean,
      fprop: Boolean,
      orbits: Int,
      crossPairs: Int,
      faceCrossPairs: Int
  ):
    def allOK: Boolean                 = parity && conn && span && fneed && fspan && fmatch && fprop
    def named: List[(String, Boolean)] =
      List(
        "parity" -> parity,
        "conn"   -> conn,
        "span"   -> span,
        "fneed"  -> fneed,
        "fspan"  -> fspan,
        "fmatch" -> fmatch,
        "fprop"  -> fprop
      )

  def verdictsOf(u: ChamberUnion): Verdicts =
    val parity = (0 until u.size)
      .groupBy(c => (u.m01(c), u.m23(c), u.cell(c)))
      .values
      .forall(_.size % 2 == 0)

    val orbits = orbitsOf(u)
    val partOf = orbits.map(o => u.orbit(o.head))
    val nParts = u.orbit.max + 1
    // cross-part intertwined orbit pairs, grouped by part pair
    val cross  = (for
      a <- orbits.indices
      b <- orbits.indices
      if a < b && partOf(a) != partOf(b) && intertwines(u, orbits(a), orbits(b))
    yield (a, b)).toVector
    val edges  = cross.map((a, b) => Set(partOf(a), partOf(b))).toSet

    val conn =
      val reach = collection.mutable.Set(0)
      var grew  = true
      while grew do
        grew = false
        for e <- edges if e.exists(reach) && !e.forall(reach) do
          reach ++= e
          grew = true
      reach.size == nParts

    // spanning tree of parts realizable with pairwise orbit-disjoint pairs from `pairs`; every spanning
    // tree rooted at part 0 has an edge order where each edge attaches a new part to the grown component,
    // so growing from part 0 is complete (compOf generalizes to pre-merged components, identity by default)
    def spanWith(pairs: Vector[(Int, Int)], compOf: Int => Int, comps: Int, used0: Set[Int]): Boolean =
      val failed                                         = collection.mutable.Set.empty[(Set[Int], Set[Int])]
      def grow(parts: Set[Int], used: Set[Int]): Boolean =
        if parts.size == comps then true
        else if failed((parts, used)) then false
        else
          val ok = pairs.exists { (a, b) =>
            !used(a) && !used(b) && {
              val (pa, pb) = (compOf(partOf(a)), compOf(partOf(b)))
              (parts(pa) != parts(pb)) && grow(parts + pa + pb, used + a + b)
            }
          }
          if !ok then failed += ((parts, used))
          ok
      grow(Set(compOf(0)), used0)

    val span = conn && spanWith(cross, identity, nParts, Set.empty)

    // the face-closure tier: only pairings passing the pair-local face check are admissible; the option
    // universe for matching includes SAME-part orbit pairs (σ₀ may pair any two orbits) — only the
    // spanning layer is restricted to cross-part pairs
    val facePairs = (for
      a <- orbits.indices
      b <- orbits.indices
      if a < b && crossFeasible(u, orbits(a), orbits(b))
    yield (a, b)).toVector
    val faceCross = facePairs.filter((a, b) => partOf(a) != partOf(b))
    val selfOk    = orbits.indices.map(i => selfFeasible(u, orbits(i))).toVector
    val fneed     = orbits.indices.forall(i => selfOk(i) || facePairs.exists((a, b) => a == i || b == i))
    val fspan     = fneed && spanWith(faceCross, identity, nParts, Set.empty)

    // orbits with NO face-feasible self-pairing must pair off on pairwise disjoint face-feasible pairs
    // (same-part allowed), and the chosen pairs plus further disjoint cross-part ones must still span the
    // parts (node-capped: a cap hit passes conservatively, keeping the criterion a sound necessary
    // condition)
    val fmatch =
      if !fspan then false
      else
        val mustCross                                                          = orbits.indices.filterNot(selfOk).toVector
        val partners                                                           = mustCross
          .map(i => facePairs.collect { case (a, b) if a == i => b; case (a, b) if b == i => a })
        var nodes                                                              = 0L
        def spanning(edges: List[(Int, Int)], used: Set[Int]): Boolean         =
          val comp              = Array.tabulate(nParts)(identity)
          def find(x: Int): Int = if comp(x) == x then x else { comp(x) = find(comp(x)); comp(x) }
          for (a, b) <- edges do
            val (ra, rb) = (find(partOf(a)), find(partOf(b)))
            if ra != rb then comp(ra) = rb
          val roots             = (0 until nParts).map(find)
          val remap             = roots.distinct.zipWithIndex.toMap
          spanWith(faceCross, p => remap(roots(p)), remap.size, used)
        def assign(idx: Int, used: Set[Int], edges: List[(Int, Int)]): Boolean =
          nodes += 1
          if nodes > 200000L then true // cap: conservative pass
          else if idx == mustCross.size then spanning(edges, used)
          else
            val i = mustCross(idx)
            if used(i) then assign(idx + 1, used, edges)
            else partners(idx).exists(j => !used(j) && assign(idx + 1, used + i + j, (i, j) :: edges))
        assign(0, Set.empty, Nil)

    val fprop = fmatch && fpropOk(u, orbits)

    Verdicts(parity, conn, span, fneed, fspan, fmatch, fprop, orbits.size, cross.size, faceCross.size)

  private def solvable(u: ChamberUnion): Boolean =
    val (sols, capped) = enumerateSigma0(u, cap = 1)
    capped || sols.nonEmpty

  /** DFS failure profile of one union: where does the search actually die? An instrumented copy of
    * enumerateSigma0's backtracker (autos-free, stops at the first solution) counting prunes by reason —
    * compat (decoration/collision in propagation), face (a (σ₀σ₁)-cycle closing at a bad length), dead (a
    * σ₀-complete proper component), disc (complete assignments rejected by final connectivity) — plus the
    * deepest assigned-chamber count reached. The measurement behind any next criterion tier: the dominant
    * prune of the criterion-OK empty tuples is the mechanism a stronger criterion must capture.
    */
  final private case class Autopsy(
      solvable: Boolean,
      nodes: Long,
      maxDepth: Int,
      pruneCompat: Long,
      pruneFace: Long,
      pruneDead: Long,
      disconnected: Long
  )

  private def autopsyOf(u: ChamberUnion): Autopsy =
    val n                 = u.size
    val sigma             = Array.fill(n)(-1)
    var found             = false
    var nodes             = 0L
    var pc                = 0L
    var pf                = 0L
    var pd                = 0L
    var dc                = 0L
    var assigned          = 0
    var maxDepth          = 0
    val classIdx          = collection.mutable.Map.empty[(Int, Int, Int), Int]
    val classOf           = Array.tabulate(n) { c =>
      classIdx.getOrElseUpdate((u.m01(c), u.m23(c), u.cell(c)), classIdx.size)
    }
    val classMembers      = Vector.tabulate(classIdx.size)(k => (0 until n).filter(classOf(_) == k).toVector)
    val unassignedInClass = Array.tabulate(classIdx.size)(classMembers(_).size)

    def faceOk(c: Int): Boolean =
      var a     = c
      var steps = 0
      while true do
        val b = sigma(a)
        if b < 0 then return true
        a = u.s1(b)
        steps += 1
        if a == c then return u.m01(c) % steps == 0
        if steps > u.m01(c) then return false
      false

    def undoAll(done: List[Int]): Unit =
      done.foreach { x =>
        sigma(x) = -1
        assigned -= 1
        unassignedInClass(classOf(x)) += 1
      }

    def assign(c0: Int, d0: Int): Option[List[Int]] =
      var todo            = List((c0, d0))
      var done: List[Int] = Nil
      var ok              = true
      while todo.nonEmpty && ok do
        val (c, d) = todo.head
        todo = todo.tail
        if sigma(c) == d && sigma(d) == c then ()
        else if sigma(c) != -1 || sigma(d) != -1 then { ok = false; pc += 1 }
        else if !(u.m01(c) == u.m01(d) && u.m23(c) == u.m23(d) && u.cell(c) == u.cell(d)) then
          ok = false
          pc += 1
        else
          sigma(c) = d
          sigma(d) = c
          assigned += (if c == d then 1 else 2)
          unassignedInClass(classOf(c)) -= 1
          if d != c then unassignedInClass(classOf(d)) -= 1
          done = if c == d then c :: done else c :: d :: done
          todo = (u.s2(c), u.s2(d)) :: (u.s3(c), u.s3(d)) :: todo
      if ok then
        if done.forall(faceOk) then
          maxDepth = math.max(maxDepth, assigned)
          Some(done)
        else { pf += 1; undoAll(done); None }
      else { undoAll(done); None }

    def connected(): Boolean =
      val seen  = collection.mutable.Set(0)
      var front = List(0)
      while front.nonEmpty do
        front = front.flatMap(c =>
          List(sigma(c), u.s1(c), u.s2(c), u.s3(c)).filter(x => x >= 0 && seen.add(x))
        )
      seen.size == n

    def deadClosedComponent(): Boolean =
      val seen = Array.fill(n)(false)
      var c0   = 0
      var dead = false
      while c0 < n && !dead do
        if !seen(c0) then
          var open  = false
          var size  = 0
          var front = List(c0)
          seen(c0) = true
          while front.nonEmpty do
            val x = front.head
            front = front.tail
            size += 1
            if sigma(x) < 0 then open = true
            for y <- List(u.s1(x), u.s2(x), u.s3(x), sigma(x)) do
              if y >= 0 && !seen(y) then
                seen(y) = true
                front = y :: front
          dead = !open && size < n
        c0 += 1
      dead

    def bt(): Unit =
      if found then return
      nodes += 1
      if deadClosedComponent() then { pd += 1; return }
      var c    = -1
      var best = Int.MaxValue
      var i    = 0
      while i < n do
        if sigma(i) == -1 && unassignedInClass(classOf(i)) < best then
          best = unassignedInClass(classOf(i))
          c = i
        i += 1
      if c == -1 then
        if connected() then found = true else dc += 1
      else
        for d <- classMembers(classOf(c)) if sigma(d) == -1 && !found do
          assign(c, d) match
            case Some(done) =>
              bt()
              undoAll(done)
            case None       => ()

    bt()
    Autopsy(found, nodes, maxDepth, pc, pf, pd, dc)

  /** Autopsy mode: same stratum args, prints the DFS failure profile per tuple and aggregates by
    * (criteria-OK, solvable) group — the residual group (criteria OK yet empty) is the target.
    */
  private def autopsyMode(args: Vector[String], say: String => Unit): Unit =
    for g <- args.grouped(2) do
      val sps     = g(0).split(":").toVector.map(_.toInt).sorted
      val orders  = g(1).split(":").toVector.map(_.toInt)
      val syms    = sps.map(symmetryOf)
      val allSubs = sps.indices.toVector
        .foldLeft(Vector(Vector.empty[Set[StarFoldings.Perm]])) { (acc, r) =>
          for t <- acc; sub <- subgroupsOfSpecies(sps(r)) if sub.size == orders(r) yield t :+ sub
        }
      say(s"== AUTOPSY ${sps.mkString(":")} orders ${orders.mkString(",")}: ${allSubs.size} tuples ==")
      val groups  =
        collection.mutable.Map.empty[(Boolean, Boolean), Vector[Autopsy]].withDefaultValue(Vector.empty)
      for subs <- allSubs do
        val fs = sps.indices.toVector.map(r => fold(syms(r), subs(r)))
        val u  = unionOf(fs)
        val v  = verdictsOf(u)
        val a  = autopsyOf(u)
        groups((v.allOK, a.solvable)) = groups((v.allOK, a.solvable)) :+ a
      for ((ok, solv), as) <- groups.toVector.sortBy((k, _) => k) do
        val label                      = (if ok then "critOK " else "critBAD") + (if solv then "+solvable" else "+empty   ")
        def mean(f: Autopsy => Double) = as.map(f).sum / as.size
        say(f"  $label n=${as.size}%4d  nodes=${mean(_.nodes.toDouble)}%9.0f  " +
          f"maxDepth=${mean(_.maxDepth.toDouble)}%5.1f  " +
          f"prunes compat=${mean(_.pruneCompat.toDouble)}%9.0f face=${mean(_.pruneFace.toDouble)}%9.0f " +
          f"dead=${mean(_.pruneDead.toDouble)}%7.0f disc=${mean(_.disconnected.toDouble)}%5.0f")

  /** Set-level broad sampling, in the manner of a parity sweep: criteria per residue tuple, DFS verification
    * (criterion-OK AND criterion-BAD tuples — the BAD ones are the soundness check) on unions ≤
    * -Dorbit.dfsmax.
    */
  private def setMode(args: Vector[String], say: String => Unit): Unit =
    val lo     = args(0).toInt
    val dfsMax = sys.props.get("orbit.dfsmax").map(_.toInt).getOrElse(200)
    for a <- args.drop(1) do
      val sps      = a.split(":").toVector.map(_.toInt).sorted
      val syms     = sps.map(symmetryOf)
      val trivial  = sps.map(i => symmetryOf(i).cx.chambers.size).sum
      val tuples   = CensusBands.bandTuples(sps, lo, trivial)
      var badEmpty = 0
      var badBig   = 0
      var badSolv  = 0 // SOUNDNESS VIOLATIONS
      var okEmpty  = 0
      var okSolv   = 0
      var okBig    = 0
      var done     = 0
      val byCrit   = collection.mutable.Map.empty[String, Int].withDefaultValue(0)
      val t0       = System.currentTimeMillis
      for subs <- tuples do
        done += 1
        if done % 200 == 0 then
          say(s"  [${(System.currentTimeMillis - t0) / 1000}s] ${sps.mkString(":")} $done/${tuples.size} " +
            s"(badEmpty=$badEmpty badSolv=$badSolv okEmpty=$okEmpty okSolv=$okSolv " +
            s"okBig=$okBig badBig=$badBig)")
        val fs = sps.indices.toVector.map(r => fold(syms(r), subs(r)))
        val u  = unionOf(fs)
        val v  = verdictsOf(u)
        for (name, ok) <- v.named if !ok do byCrit(name) += 1
        if v.allOK then
          if u.size > dfsMax then okBig += 1
          else if solvable(u) then okSolv += 1
          else okEmpty += 1
        else if u.size > dfsMax then badBig += 1
        else if solvable(u) then
          badSolv += 1
          say(s"  SOUNDNESS VIOLATION ${sps.mkString(":")} |H|=${subs.map(_.size).mkString(",")} " +
            s"union=${u.size} ${v.named.map((n, ok) => s"$n=$ok").mkString(" ")} but DFS-SOLVABLE")
        else badEmpty += 1
      say(f"${sps.mkString(":")}%-14s tuples=${tuples.size}%6d  critBAD+empty=$badEmpty%6d " +
        f"critBAD+SOLVABLE=$badSolv%3d  critOK+empty=$okEmpty%6d  critOK+solvable=$okSolv%4d  " +
        f"unverified(>$dfsMax) ok=$okBig%5d bad=$badBig%5d  " +
        s"[BAD by: ${List("parity", "conn", "span", "fneed", "fspan", "fmatch", "fprop")
            .map(n => s"$n=${byCrit(n)}").mkString(" ")}]")

  /** Pairwise conn-table mode (`pairs <i> <j> ...`): the β1 reduction. A conn edge depends only on the two
    * folded parts, so per species pair the finite table over both subgroup lattices — does the 2-part union
    * carry a cross-part intertwiner? — decides the conn edges of every tuple of every set containing the
    * pair. Aggregated by folding-order pair: total subgroup pairs, pairs WITH an edge, and the 2-part
    * union-size range of the edged cells (edges living only below the residue threshold cannot rescue residue
    * tuples). An all-zero region + the soundness theorem closes strata with no per-tuple work.
    */
  private def pairsMode(args: Vector[String], say: String => Unit): Unit =
    for g <- args.grouped(2) do
      val (i, j)         = (g(0).toInt, g(1).toInt)
      val (si, sj)       = (symmetryOf(i), symmetryOf(j))
      val (subsI, subsJ) = (subgroupsOfSpecies(i), subgroupsOfSpecies(j))
      say(s"== pair $i ~ $j  [${SpeciesCorona.label(i)} ~ ${SpeciesCorona.label(j)}]  " +
        s"${subsI.size} x ${subsJ.size} subgroup pairs ==")
      // (|Ha|, |Hb|) -> (cells, edged cells, min union with edge, max union with edge, max crossPairs)
      val agg            = collection.mutable.Map.empty[(Int, Int), (Int, Int, Int, Int, Int)]
      for ha <- subsI; hb <- subsJ do
        val u                  = unionOf(Vector(fold(si, ha), fold(sj, hb)))
        val orbits             = orbitsOf(u)
        val partOf             = orbits.map(o => u.orbit(o.head))
        val cnt                = (for
          a <- orbits.indices
          b <- orbits.indices
          if a < b && partOf(a) != partOf(b) && intertwines(u, orbits(a), orbits(b))
        yield 1).size
        val key                = (ha.size, hb.size)
        val (c, e, lo, hi, mx) = agg.getOrElse(key, (0, 0, Int.MaxValue, -1, 0))
        agg(key) =
          if cnt > 0 then (c + 1, e + 1, math.min(lo, u.size), math.max(hi, u.size), math.max(mx, cnt))
          else (c + 1, e, lo, hi, mx)
      for ((oa, ob), (c, e, lo, hi, mx)) <- agg.toVector.sortBy((k, _) => k) do
        val range = if e > 0 then f"union[$lo%3d,$hi%3d] maxEdges=$mx%3d" else "               --"
        say(f"  |Ha|=$oa%3d |Hb|=$ob%3d  cells=$c%4d  edged=$e%4d  $range")
      val edgedTotal     = agg.values.map(_._2).sum
      val cellTotal      = agg.values.map(_._1).sum
      say(f"  TOTAL cells=$cellTotal  edged=$edgedTotal (${100.0 * edgedTotal / cellTotal}%.1f%%)")

  /** Conn-gated fprop sweep (the 10–20× cut measurement): `fpropsweep <lo> <setfile>` — per set, stream the
    * residue band, gate every tuple through the β1 conn tables (identity-hash canonical index maps,
    * microseconds, no folds built), and run the full criteria stack only on the conn-OK survivors. The
    * conn-BAD mass is theorem-certified and skipped; table conn and recomputed conn are cross-asserted on
    * every survivor (a GATE MISMATCH line would indicate canonicalization drift — must never appear).
    */
  private def fpropSweepMode(args: Vector[String], say: String => Unit): Unit =
    val lo       = args(0).toInt
    val sets     = scala.io.Source.fromFile(args(1)).getLines().map(_.trim).filter(_.nonEmpty).toVector
    var gBand    = 0L
    var gConnBad = 0L
    var gSurv    = 0L
    val gByCrit  = collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
    val t0       = System.currentTimeMillis
    def dt       = (System.currentTimeMillis - t0) / 1000
    for (a, si) <- sets.zipWithIndex do
      val sps      = a.split(":").toVector.map(_.toInt).sorted
      val k        = sps.size
      val syms     = sps.map(symmetryOf)
      val trivial  = sps.map(i => symmetryOf(i).cx.chambers.size).sum
      val canonIdx = sps.map { i =>
        val m = new java.util.IdentityHashMap[Set[StarFoldings.Perm], Integer]
        for (sub, ix) <- ConnGate.canonicalSubs(i).zipWithIndex do m.put(sub, ix)
        m
      }
      val gate     = ConnGate.of(sps, say)
      val tuples   = CensusBands.bandTuples(sps, lo, trivial)
      var connBad  = 0L
      var surv     = 0L
      val byCrit   = collection.mutable.Map.empty[String, Long].withDefaultValue(0L)
      for subs <- tuples do
        val idx = Vector.tabulate(k)(r => canonIdx(r).get(subs(r)).intValue)
        if !gate.connOK(idx) then connBad += 1
        else
          surv += 1
          val fs = sps.indices.toVector.map(r => fold(syms(r), subs(r)))
          val u  = unionOf(fs)
          val v  = verdictsOf(u)
          if !v.conn then
            say(s"  GATE MISMATCH ${sps.mkString(":")} |H|=${subs.map(_.size).mkString(",")} " +
              s"union=${u.size}: table conn-OK but recomputed conn-BAD")
          for (name, ok) <- v.named if !ok do byCrit(name) += 1
          if surv % 2000 == 0 then
            say(s"  [${dt}s] set ${si + 1}/${sets.size} ${sps.mkString(":")} " +
              s"survivors $surv (fprop-flagged ${byCrit("fprop")})")
      gBand += tuples.size
      gConnBad += connBad
      gSurv += surv
      for (n, c) <- byCrit do gByCrit(n) += c
      if surv > 0 || (si + 1) % 100 == 0 then
        say(f"[${dt}s] ${sps.mkString(":")}%-16s band=${tuples.size}%8d connBAD=$connBad%8d " +
          f"survivors=$surv%7d  [survivor BAD: ${List("parity", "span", "fneed", "fspan", "fmatch", "fprop")
              .map(n => s"$n=${byCrit(n)}").mkString(" ")}] (${si + 1}/${sets.size})")
    say(f"TOTAL band=$gBand%,d connBAD(gate)=$gConnBad%,d survivors=$gSurv%,d  " +
      s"[survivor BAD: ${List("parity", "span", "fneed", "fspan", "fmatch", "fprop")
          .map(n => s"$n=${gByCrit(n)}").mkString(" ")}]  [${dt}s]")

  def main(args: Array[String]): Unit =
    def say(s: String): Unit = { println(s); System.out.flush() }
    if args.headOption.contains("set") then return setMode(args.toVector.drop(1), say)
    if args.headOption.contains("autopsy") then return autopsyMode(args.toVector.drop(1), say)
    if args.headOption.contains("pairs") then return pairsMode(args.toVector.drop(1), say)
    if args.headOption.contains("fpropsweep") then return fpropSweepMode(args.toVector.drop(1), say)
    require(
      args.length >= 2 && args.length % 2 == 0,
      "usage: pairs of sps orders | set <lo> <sps>... | autopsy <sps> <orders> ..."
    )

    val summary = collection.mutable.ArrayBuffer.empty[String]
    for g <- args.grouped(2) do
      val sps     = g(0).split(":").toVector.map(_.toInt).sorted
      val orders  = g(1).split(":").toVector.map(_.toInt)
      val syms    = sps.map(symmetryOf)
      val allSubs = sps.indices.toVector
        .foldLeft(Vector(Vector.empty[Set[StarFoldings.Perm]])) { (acc, r) =>
          for t <- acc; sub <- subgroupsOfSpecies(sps(r)) if sub.size == orders(r) yield t :+ sub
        }
      say(s"== ${sps.mkString(":")} orders ${orders.mkString(",")}: ${allSubs.size} tuples ==")
      // confusion counts per criterion: (criterionOK?, solvable?) -> n
      val conf    = collection.mutable.Map.empty[(String, Boolean, Boolean), Int].withDefaultValue(0)
      var done    = 0
      val t0      = System.currentTimeMillis
      for subs <- allSubs do
        done += 1
        if done % 50 == 0 then
          say(s"  [${(System.currentTimeMillis - t0) / 1000}s] $done/${allSubs.size}")
        val fs   = sps.indices.toVector.map(r => fold(syms(r), subs(r)))
        val u    = unionOf(fs)
        val v    = verdictsOf(u)
        val solv = solvable(u)
        for (name, ok) <- v.named do
          conf((name, ok, solv)) += 1
          if !ok && solv then
            say(s"  SOUNDNESS VIOLATION $name: |H|=${subs.map(_.size).mkString(",")} union=${u.size} " +
              s"orbits=${v.orbits} crossPairs=${v.crossPairs} faceCrossPairs=${v.faceCrossPairs} " +
              s"but DFS-SOLVABLE")
      val solvN   = conf.collect { case ((n, _, true), c) if n == "parity" => c }.sum
      val empN    = conf.collect { case ((n, _, false), c) if n == "parity" => c }.sum
      summary += f"${sps.mkString(":")} ${orders.mkString(",")}%-12s tuples=${allSubs.size}%4d " +
        f"(solvable=$solvN empty=$empN)"
      for name <- List("parity", "conn", "span", "fneed", "fspan", "fmatch", "fprop") do
        summary += f"    $name%-6s  BAD+empty=${conf((name, false, false))}%4d  " +
          f"BAD+SOLVABLE=${conf((name, false, true))}%3d  OK+empty=${conf((name, true, false))}%4d  " +
          f"OK+solvable=${conf((name, true, true))}%3d"
    summary.foreach(say)
