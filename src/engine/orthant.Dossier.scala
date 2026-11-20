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
import soundness.{Holding as _, *}
import soundness.dysasymptotics.linearSize
import soundness.sortingAlgorithms.timsort

// Every version of one document: what the store holds for it at each point of a space. A
// dossier is a value; a check-in gives a new one.
case class Dossier(space: Space, holdings: Map[Point, Holding] = Map()):
  // Renditions are derived from each other, many times over, so each is computed once.
  private val memo: scala.collection.mutable.HashMap[Point, Optional[Rendition]] =
    scala.collection.mutable.HashMap()

  def hold(point: Point, holding: Optional[Holding]): Dossier =
    Dossier(space, holding.lay(holdings.omit(point))(holdings.define(point, _)))

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
      holdings.at(point).let(_.only { case Holding.Base(lines) => Rendition(lines) })

    space.storageParent(point).lay(base): parent =>
      val axis = space.storageAxis(point).or(panic(m"a point with a storage parent has an axis"))
      val origins = specific(space.origins(point))

      // The most specific holding which is not a delta decides whether the document exists,
      // unless a delta more specific than it cancels it.
      def decide(todo: List[Point], deltas: List[Point]): Optional[Rendition] = todo match
        case origin :: tail => holdings.at(origin) match
          case Holding.Base(lines)  if deltas.nil  => Rendition(lines)
          case Holding.Absent(_)    if deltas.nil  => Unset
          case Holding.Delta(_, _)                 => decide(tail, origin :: deltas)
          case Holding.Base(_) | Holding.Absent(_) => apply(deltas.reverse)
          case _                                   => decide(tail, deltas)

        case _ =>
          apply(deltas.reverse)

      def apply(deltas: List[Point]): Optional[Rendition] = content(parent).let: reference =>
        val placed: List[Placement | Issue] = deltas.bind: origin =>
          val hunks: List[Hunk] =
            holdings.at(origin).let(_.only { case Holding.Delta(_, hunks) => hunks }).or(Nil)

          val source: Optional[List[Text]] =
            if origin == point then reference else space.storageParent(origin).let(content(_))

          source.lay(List[Placement | Issue]()): source =>
            val located: List[(Int, Hunk) | Issue] = hunks.map: hunk =>
              Rebase.occurrences(hunk, source) match
                case start :: Nil => (start, hunk)
                case Nil          => Issue.Orphan(origin, hunk)
                case _            => Issue.Ambiguous(origin, hunk)

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

  private def resolve(reference: List[Text], candidates: List[Placement], issues: List[Issue])
  :   Rendition raises Axis.Error =

    // Fresh before stale, then the most specific origin; a candidate is accepted if it overlaps
    // nothing accepted before it.
    val ranked: List[Placement] =
      val byStart = candidates.order(_.start)

      val byOrigin = space.axes.reverse.fold(byStart): (candidates, axis) =>
        candidates.order: candidate => -axis.depth(candidate.origin(axis))

      byOrigin.order: candidate => if candidate.fresh then 0 else 1

    val (accepted, conflicts) =
      ranked.fold((List[Placement](), List[Issue]())): (state, candidate) =>
        val (accepted, conflicts) = state

        val met = accepted.filter(_.overlaps(candidate)).head

        met.lay((candidate :: accepted, conflicts)): rival =>
          val comparable =
            space.below(candidate.origin, rival.origin) ||
              space.below(rival.origin, candidate.origin)

          if comparable || rival.fresh != candidate.fresh || rival.lines == candidate.lines
          then (accepted, conflicts)
          else (accepted, Issue.Conflict(rival, candidate) :: conflicts)

    // An insertion comes before a replacement which begins where it is.
    val ordered =
      accepted.order: placement =>
        placement.start*2 + (if placement.start == placement.end then 0 else 1)

    def render(todo: List[Placement], position: Int, count: Int): (List[Text], List[Placement]) =
      todo match
        case placement :: tail =>
          val shared = reference.excerpt(position, placement.start)
          val from = count + shared.size
          val to = from + placement.lines.size
          val (lines, placements) = render(tail, placement.end, to)
          (shared + placement.lines + lines, placement.copy(from = from, to = to) :: placements)

        case _ =>
          (reference.excerpt(position, reference.size), Nil)

    val (lines, placements) = render(ordered, 0, 0)
    val stale = placements.filter(!_.fresh).map(Issue.Stale(_))

    Rendition(lines, reference, placements, issues + stale + conflicts.reverse)

  // The dossier in which the content at `point` is `lines`, or in which the document is absent
  // there. Only what is held at `point` differs; every point below it follows.
  def store(point: Point, lines: Optional[List[Text]]): Dossier raises Axis.Error =
    space.storageAxis(point).lay(hold(point, lines.let(Holding.Base(_)))): axis =>
      val bare = hold(point, Unset)

      def gone: Dossier =
        if bare.content(point).absent then bare else hold(point, Holding.Absent(axis.name))

      lines.lay(gone): lines =>
        val delta: Dossier = holdings.at(point) match
          case Holding.Delta(_, _) => this
          case _                   => hold(point, Holding.Delta(axis.name, Nil))

        delta.rendition(point).lay(hold(point, Holding.Base(lines))): rendition =>
          val keep = axis.preference == Preference.Own
          val hunks = Inscription.hunks(rendition, lines, point, keep)
          def held(hunks: List[Hunk]): Dossier = hold(point, Holding.Delta(axis.name, hunks))

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
        if stale.nil then this else hold(point, Holding.Delta(axis.name, hunks))

  // Whether the point's content is a translation of its storage parent's.
  def translation(point: Point): Boolean raises Axis.Error =
    space.storageAxis(point).lay(false): axis => axis.value(point(axis)).translation

  // The lines of the storage parent's content, counted from nought, which a translation has yet
  // to replace: those which are not blank, and which no hunk takes the place of.
  def pending(point: Point): List[Int] raises Axis.Error =
    if !translation(point) then Nil else rendition(point).lay(Nil): rendition =>
      def covered(line: Int): Boolean = rendition.placements.exists: placement =>
        placement.start <= line && line < placement.end

      rendition.reference.fold((0, List[Int]())): (state, line) =>
        val (index, found) = state
        (index + 1, if line == t"" || covered(index) then found else index :: found)

      . apply(1).reverse

  // The dossier in which each run of the storage parent's lines given, from `start` to `end`,
  // is held at `point` to be translated by the same lines, where no hunk replaces any of them.
  // Elsewhere than on a translation edge, such a hunk would say nothing.
  def affirm(point: Point, spans: List[(Int, Int)]): Dossier raises Axis.Error =
    space.storageAxis(point).lay(this): axis =>
      rendition(point).lay(this): rendition =>
        val free = spans.filter: (start, end) =>
          start < end && !rendition.placements.exists: placement =>
            if placement.start == placement.end
            then start < placement.start && placement.start < end
            else placement.start < end && start < placement.end

        val held = holdings.at(point) match
          case Holding.Base(_) | Holding.Absent(_) => true
          case _                                   => false

        if free.nil || held || !translation(point) then this else
          val kept: List[(Int, Hunk)] =
            rendition.placements.filter(_.origin == point)
            . map: placement => (placement.start, placement.hunk)

          val added: List[(Int, Hunk)] = free.map: (start, end) =>
            val same = rendition.reference.excerpt(start, end)
            (start, Inscription.anchor(Hunk(same, same), start, rendition.reference))

          val hunks = (kept + added).order { (start, hunk) => hunk.rank(start) }.map(_(1))
          hold(point, Holding.Delta(axis.name, hunks))

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
              val united = dossier.hold(point, Holding.Delta(axis.name, ordered))

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
      holdings.at(point) match
        case Holding.Delta(axis, hunks) =>
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
                hold(point, Holding.Delta(axis, ordered + unlocated))

        case _ =>
          this
