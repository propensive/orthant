                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Orthant, version 0.1.0.                                                                       ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package orthant

import proscenium.{List, Nil, Map, `::`}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.sortingAlgorithms.timsort

// Every version of one document: what the store holds for it at each point of a space. A
// dossier is a value; a check-in gives a new one.
case class Dossier(space: Space, folios: Map[Point, Folio] = Map()):
  // Renditions are derived from each other, many times over, so each is computed once.
  private val memo: scala.collection.mutable.HashMap[Point, Optional[Rendition]] =
    scala.collection.mutable.HashMap()

  def hold(point: Point, folio: Optional[Folio]): Dossier =
    Dossier(space, folio.lay(folios.omit(point))(folios.define(point, _)))

  // The content of the document at a point, or `Unset` if it does not exist there.
  def content(point: Point): Optional[List[Text]] raises Axis.Error = rendition(point).let(_.lines)

  def rendition(point: Point): Optional[Rendition] raises Axis.Error =
    memo.get(point).getOrElse:
      val result = derive(point)
      memo(point) = result
      result

  // The most specific first: the deeper on the earliest axis on which two points differ.
  private def specific(points: List[Point]): List[Point] raises Axis.Error =
    space.axes.reverse.fold(points): (points, axis) =>
      points.order: point => -axis.depth(point(axis))

  private def derive(point: Point): Optional[Rendition] raises Axis.Error =
    def base: Optional[Rendition] =
      folios.at(point).let(_.only { case Folio.Base(lines) => Rendition(lines)() })

    space.storageParent(point).lay(base): parent =>
      val axis = space.storageAxis(point).or(panic(m"a point with a storage parent has an axis"))
      val origins = specific(space.origins(point))

      // The most specific folio which is not a delta decides whether the document exists,
      // unless a delta more specific than it cancels it.
      def decide(todo: List[Point], deltas: List[Point]): Optional[Rendition] = todo match
        case origin :: tail => folios.at(origin) match
          case Folio.Base(lines)  if deltas.nil => Rendition(lines)()
          case Folio.Absent(_)    if deltas.nil => Unset
          case Folio.Delta(_, _)                => decide(tail, origin :: deltas)
          case Folio.Base(_) | Folio.Absent(_)  => apply(deltas.reverse)
          case _                                => decide(tail, deltas)

        case _ =>
          apply(deltas.reverse)

      def apply(deltas: List[Point]): Optional[Rendition] = rendition(parent).let(_.content).let: reference =>
        val placed: List[Placement | Issue] = deltas.bind: origin =>
          val hunks: List[Hunk] =
            folios.at(origin).let(_.only { case Folio.Delta(_, hunks) => hunks }).or(Nil)

          val source: Optional[Content] =
            if origin == point then reference
            else space.storageParent(origin).let(rendition(_)).let(_.content)

          source.lay(List[Placement | Issue]()): source =>
            val located: List[(Int, Hunk) | Issue] = hunks.map: hunk =>
              Rebase.locate(hunk, source) match
                case Content.Absent  => Issue.Orphan(origin, hunk)
                case Content.Several => Issue.Ambiguous(origin, hunk)
                case start           => (start, hunk)

            val outcomes: List[Rebase.Outcome] =
              val found = located.sweep { case pair: (Int, Hunk) @unchecked => pair }

              if origin == point then found.map(Rebase.Outcome.Fresh(_, _))
              else Rebase(found, source, reference)

            located.sweep { case issue: Issue if origin == point => issue } +
              outcomes.sweep:
                case Rebase.Outcome.Fresh(start, hunk) =>
                  place(start, start + hunk.replaced.size, hunk, origin, !hunk.stale, axis)

                case Rebase.Outcome.Stale(start, end, hunk) =>
                  place(start, end, hunk, origin, false, axis)

        resolve(reference, placed.sweep { case placement: Placement => placement },
                placed.sweep { case issue: Issue => issue })

      decide(origins, Nil)

  // A stale hunk shows what its axis prefers: its own lines, or the lines it would replace.
  private def place(start: Int, end: Int, hunk: Hunk, origin: Point, fresh: Boolean, axis: Axis)
  :   Placement =

    val own = fresh || axis.preference == Preference.Own
    Placement(start, end, hunk, origin, fresh, if own then hunk.lines else hunk.replaced)

  private def resolve(reference: Content, candidates: List[Placement], issues: List[Issue])
  :   Rendition raises Axis.Error =

    val ranked: List[Placement] =
      val byStart = candidates.order(_.start)

      val byOrigin = space.axes.reverse.fold(byStart): (candidates, axis) =>
        candidates.order: candidate => -axis.depth(candidate.origin(axis))

      byOrigin.order: candidate => if candidate.fresh then 0 else 1

    // The accepted placements overlap one another nowhere, so those which replace lines are
    // sought by where they begin, and those which only insert by where they insert. Each is
    // numbered as it is accepted: of several which a candidate overlaps, the latest is its rival.
    val spans = java.util.TreeMap[Integer, (Int, Placement)]()
    val points = java.util.TreeMap[Integer, (Int, Placement)]()
    var accepted: scala.List[Placement] = scala.Nil
    var conflicts: scala.List[Issue] = scala.Nil
    var number = 0

    def rival(candidate: Placement): Optional[Placement] =
      var best: Optional[(Int, Placement)] = Unset

      def offer(found: (Int, Placement) | Null): Unit =
        if found != null then
          val pair = found.asInstanceOf[(Int, Placement)]
          if pair(1).overlaps(candidate) && best.lay(true)(_(0) < pair(0)) then best = pair

      def valueOf(entry: java.util.Map.Entry[Integer, (Int, Placement)] | Null)
      :   (Int, Placement) | Null =

        if entry == null then null else entry.getValue

      if candidate.start == candidate.end then
        offer(points.get(Integer.valueOf(candidate.start)))
        offer(valueOf(spans.lowerEntry(Integer.valueOf(candidate.start))))
      else
        offer(valueOf(spans.lowerEntry(Integer.valueOf(candidate.start))))

        spans.subMap(Integer.valueOf(candidate.start), true, Integer.valueOf(candidate.end), false)
        . nn.values.nn.forEach(offer(_))

        points.subMap(Integer.valueOf(candidate.start), false, Integer.valueOf(candidate.end), false)
        . nn.values.nn.forEach(offer(_))

      best.let(_(1))

    ranked.each: candidate =>
      rival(candidate).lay
        ( {
            number += 1
            accepted = candidate :: accepted

            if candidate.start == candidate.end
            then points.put(Integer.valueOf(candidate.start), (number, candidate))
            else spans.put(Integer.valueOf(candidate.start), (number, candidate))
            () } ): rival =>
        val comparable =
          space.below(candidate.origin, rival.origin) ||
            space.below(rival.origin, candidate.origin)

        if !(comparable || rival.fresh != candidate.fresh || rival.lines == candidate.lines)
        then conflicts = Issue.Conflict(rival, candidate) :: conflicts

    val ordered =
      List.from(accepted).order: placement =>
        placement.start*2 + (if placement.start == placement.end then 0 else 1)

    val buffer = scala.collection.mutable.ListBuffer[Text]()
    val placed = scala.collection.mutable.ListBuffer[Placement]()
    var position = 0
    var count = 0

    ordered.each: placement =>
      val shared = reference.slice(position, placement.start).stdlib
      buffer ++= shared
      val from = count + shared.length
      val size = placement.lines.stdlib.length
      buffer ++= placement.lines.stdlib
      placed += placement.copy(from = from, to = from + size)
      count = from + size
      position = placement.end

    buffer ++= reference.slice(position, reference.size).stdlib
    val lines = List.from(buffer.toList)
    val placements = List.from(placed.toList)
    val stale = placements.filter(!_.fresh).map(Issue.Stale(_))

    Rendition(lines, reference.lines, placements, issues + stale + List.from(conflicts.reverse))(reference)

  def store(point: Point, lines: Optional[List[Text]]): Dossier raises Axis.Error =
    space.storageAxis(point).lay(hold(point, lines.let(Folio.Base(_)))): axis =>
      val bare = hold(point, Unset)

      def gone: Dossier =
        if bare.content(point).absent then bare else hold(point, Folio.Absent(axis.name))

      lines.lay(gone): lines =>
        val delta: Dossier = folios.at(point) match
          case Folio.Delta(_, _) => this
          case _                 => hold(point, Folio.Delta(axis.name, Nil))

        delta.rendition(point).lay(hold(point, Folio.Base(lines))): rendition =>
          val keep = axis.preference == Preference.Own
          val hunks = Inscription.hunks(rendition, lines, point, keep)
          def held(hunks: List[Hunk]): Dossier = hold(point, Folio.Delta(axis.name, hunks))

          // A hunk without which the content is the same, and no more of it wants attention,
          // says nothing an inherited one does not.
          def same(dossier: Dossier): Boolean =
            dossier.content(point) == lines &&
              dossier.issues(point).size <= held(hunks).issues(point).size &&
              dossier.pending(point).size <= held(hunks).pending(point).size

          // Dropping one may leave another with nothing to do, so this repeats until none goes.
          def prune(hunks: List[Hunk]): List[Hunk] =
            val pruned = hunks.fold(hunks): (hunks, hunk) =>
              val fewer = hunks.filter(_ != hunk)
              if same(held(fewer)) then fewer else hunks

            if pruned == hunks then hunks else prune(pruned)

          val needed = prune(hunks)

          if needed.nil && same(bare) then bare else held(needed)

  // What wants attention in the content at a point.
  def issues(point: Point): List[Issue] raises Axis.Error = rendition(point).let(_.issues).or(Nil)

  // The dossier in which the stale hunks at `point` have been checked: each is stored there as
  // it is shown, against the lines its storage parent now has, and is stale no longer.
  // Only those which `select` chooses are checked; the others stay as they are.
  def confirm(point: Point, select: Placement => Boolean = placement => true)
  :   Dossier raises Axis.Error =

    space.storageAxis(point).lay(this): axis =>
      rendition(point).lay(this): rendition =>
        val stale = rendition.placements.filter: placement => !placement.fresh && select(placement)

        val checked: List[(Int, Hunk)] = stale.map: placement =>
          val hunk =
            Hunk(rendition.reference.excerpt(placement.start, placement.end), placement.lines)

          (placement.start, Inscription.anchor(hunk, placement.start, rendition.reference))

        val kept: List[(Int, Hunk)] =
          rendition.placements.filter: placement =>
            placement.origin == point && !stale.has(placement)

          . map: placement => (placement.start, placement.hunk)

        val hunks = (kept + checked).order { (start, hunk) => hunk.rank(start) }.map(_(1))
        if stale.nil then this else hold(point, Folio.Delta(axis.name, hunks))

  // Whether the point's content is a translation of its storage parent's.
  def translation(point: Point): Boolean raises Axis.Error =
    space.storageAxis(point).lay(false): axis => axis.value(point(axis)).translation

  // The lines of the storage parent's content, counted from nought, which a translation has yet
  // to replace: those which are not blank, and which no hunk takes the place of.
  def pending(point: Point): List[Int] raises Axis.Error =
    if !translation(point) then Nil else rendition(point).lay(Nil): rendition =>
      val reference = rendition.base
      val covered = new scala.Array[Boolean](reference.size)

      rendition.placements.each: placement =>
        var line = placement.start

        while line < placement.end && line < covered.length do
          covered(line) = true
          line += 1

      val found = scala.collection.mutable.ListBuffer[Int]()
      var index = 0

      while index < covered.length do
        if reference(index) != t"" && !covered(index) then found += index
        index += 1

      List.from(found.toList)

  def affirm(point: Point, spans: List[(Int, Int)]): Dossier raises Axis.Error =
    space.storageAxis(point).lay(this): axis =>
      rendition(point).lay(this): rendition =>
        val free = spans.filter: (start, end) =>
          start < end && !rendition.placements.exists: placement =>
            if placement.start == placement.end
            then start < placement.start && placement.start < end
            else placement.start < end && start < placement.end

        val held = folios.at(point) match
          case Folio.Base(_) | Folio.Absent(_) => true
          case _                               => false

        if free.nil || held || !translation(point) then this else
          val kept: List[(Int, Hunk)] =
            rendition.placements.filter(_.origin == point)
            . map: placement => (placement.start, placement.hunk)

          val added: List[(Int, Hunk)] = free.map: (start, end) =>
            val same = rendition.reference.excerpt(start, end)
            (start, Inscription.anchor(Hunk(same, same), start, rendition.reference))

          val hunks = (kept + added).order { (start, hunk) => hunk.rank(start) }.map(_(1))
          hold(point, Folio.Delta(axis.name, hunks))

  // The dossier in which a hunk stored at `point` which only takes out the lines `start` to
  // `end` of its storage parent's content, and one stored there which only puts lines in at
  // either end of them, are one hunk which replaces them: lines once left out of a translation,
  // and since translated. If that would change the content of any point, or what wants
  // attention there, they stay as they are.
  def unite(point: Point, spans: List[(Int, Int)]): Dossier raises Axis.Error =
    def one(dossier: Dossier, span: (Int, Int)): Dossier =
      dossier.space.storageAxis(point).lay(dossier): axis =>
        dossier.rendition(point).lay(dossier): rendition =>
          val own =
            rendition.placements.filter: placement => placement.origin == point && placement.fresh

          val taking = own.filter: placement =>
            placement.start == span(0) && placement.end == span(1) &&
              placement.start < placement.end && placement.hunk.lines.nil

          . head

          val putting = own.filter: placement =>
            placement.start == placement.end && !placement.hunk.lines.nil &&
              (placement.start == span(0) || placement.start == span(1))

          . head

          taking.lay(dossier): taking =>
            putting.lay(dossier): putting =>
              val whole =
                Hunk(rendition.reference.excerpt(taking.start, taking.end), putting.hunk.lines)

              val hunks: List[(Int, Hunk)] =
                rendition.placements.filter: placement =>
                  placement.origin == point && placement != taking && placement != putting

                . map { placement => (placement.start, placement.hunk) } +
                  List((taking.start, Inscription.anchor(whole, taking.start, rendition.reference)))

              val ordered = hunks.order { (start, hunk) => hunk.rank(start) }.map(_(1))
              val united = dossier.hold(point, Folio.Delta(axis.name, ordered))

              val same = !space.points.exists: other =>
                united.content(other) != dossier.content(other) ||
                  united.issues(other).size != dossier.issues(other).size ||
                  united.pending(other).size > dossier.pending(other).size

              if same then united else dossier

    spans.fold(this)(one)

  // The dossier after a check-in of `lines` at `point`, in which only the points within `scope`
  // may differ in content: any other point below `point` whose content would change is pinned to
  // what it was. Hunks stored below `point` follow the change to their storage parents.
  def checkin(point: Point, lines: Optional[List[Text]], scope: Point => Boolean = point => true)
  :   Dossier raises Axis.Error raises Checkin.Error =

    val below = space.points.filter: other => other != point && space.below(other, point)

    val result = below.fold(store(point, lines)): (dossier, other) =>
      val followed = dossier.follow(this, other)

      if scope(other) || followed.content(other) == content(other) then followed
      else followed.store(other, content(other))

    if result.content(point) != lines then abort(Checkin.Error(point, Checkin.Reason.Unequal))

    below.each: other =>
      if !scope(other) && result.content(other) != content(other)
      then abort(Checkin.Error(other, Checkin.Reason.Scope))

    space.points.each: other =>
      val unplaced = result.rendition(other).let(_.issues).or(Nil).exists:
        case Issue.Orphan(origin, _)    => origin == other
        case Issue.Ambiguous(origin, _) => origin == other
        case _                          => false

      if unplaced then abort(Checkin.Error(other, Checkin.Reason.Unplaced))

    result

  // This dossier, in which the hunks stored at `point` have followed its storage parent's content
  // from what it was in `former` to what it is now. A hunk whose lines were deleted is dropped.
  private def follow(former: Dossier, point: Point): Dossier raises Axis.Error =
    space.storageParent(point).lay(this): parent =>
      folios.at(point) match
        case Folio.Delta(axis, hunks) =>
          former.content(parent).lay(this): old =>
            content(parent).lay(this): next =>
              if old == next then this else
                val located: List[(Int, Hunk)] = hunks.bind: hunk =>
                  Rebase.occurrences(hunk, old) match
                    case start :: Nil => List((start, hunk))
                    case _            => Nil

                val unlocated = hunks.filter: hunk => !located.exists(_(1) == hunk)

                val moved: List[(Int, Hunk)] = Rebase(located, old, next).sweep:
                  case Rebase.Outcome.Fresh(start, hunk) =>
                    // An insertion must keep a line of context, or the document's start.
                    val anchored = !hunk.insertion || !hunk.before.nil || hunk.start

                    if anchored && Rebase.occurrences(hunk, next) == List(start) then (start, hunk)
                    else (start, Inscription.anchor(hunk, start, next))

                  case Rebase.Outcome.Stale(start, _, hunk) =>
                    (start, Inscription.anchor(hunk, start, next))

                val ordered = moved.order { (start, hunk) => hunk.rank(start) }.map(_(1))
                hold(point, Folio.Delta(axis, ordered + unlocated))

        case _ =>
          this
