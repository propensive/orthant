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

import scala.annotation.tailrec

import proscenium.{List, Nil, Map, `::`}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.sortingAlgorithms.timsort

// Every version of one document: what the store holds for it at each point of a space. A
// dossier is a value; a check-in gives a new one. `source` is where its content comes from at
// the source points of a space which has a sourced axis.
case class Dossier
  ( space: Space, folios: Map[Point, Folio] = Map(), source: Optional[Source] = Unset ):
  // Renditions are derived from each other, many times over, so each is computed once.
  private val memo: scala.collection.mutable.HashMap[Point, Optional[Rendition]] =
    scala.collection.mutable.HashMap()

  private val unplaced: scala.collection.mutable.HashMap[Point, List[Int]] =
    scala.collection.mutable.HashMap()

  def hold(point: Point, folio: Optional[Folio]): Dossier =
    Dossier(space, folio.lay(folios.omit(point))(folios.define(point, _)), source)

  // As `hold`, but what is already derived at the points which do not inherit from `point` is
  // kept, as it is unchanged.
  private def retain(point: Point, folio: Optional[Folio]): Dossier raises Axis.Error =
    val result = hold(point, folio)

    memo.foreach: (other, rendition) =>
      if !space.below(other, point) then result.memo(other) = rendition

    unplaced.foreach: (other, lines) =>
      if !space.below(other, point) then result.unplaced(other) = lines

    result

  // The hunks which are stored at a point, each with the lines of its anchor: those whose
  // anchors are found, once, in the content of the point's storage parent.
  def hunks(point: Point): List[Hunk] raises Axis.Error =
    val parent = space.storageParent(point).let(rendition(_)).let(_.content)

    folios.at(point).let(_.only { case Folio.Delta(_, hunks) => hunks }).or(Nil).bind: hunk =>
      parent.lay(List[Hunk]()): content =>
        val start = Rebase.locate(hunk, content)
        if start < 0 then Nil else List(hunk.open(content, start))

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

    // At a source point the content is what the source has, whatever is stored.
    def sourced: Optional[Rendition] =
      space.sourced.let { axis => source.let(_.content(point(axis))) }.let(Rendition(_)())

    if space.source(point) then sourced else space.storageParent(point).lay(base): parent =>
      val axis = space.storageAxis(point).or(panic(m"a point with a storage parent has an axis"))
      val origins = specific(space.origins(point))

      // The most specific folio which is not a delta decides whether the document exists,
      // unless a delta more specific than it cancels it.
      def decide(todo: List[Point], deltas: List[Point]): Optional[Rendition] = todo match
        case origin :: tail => folios.at(origin) match
          case Folio.Base(lines)  if deltas.nil => outright(origin, lines)
          case Folio.Absent(_)    if deltas.nil => Unset
          case Folio.Delta(_, _)                => decide(tail, origin :: deltas)
          case Folio.Base(_) | Folio.Absent(_)  => apply(deltas.reverse)
          case _                                => decide(tail, deltas)

        case _ =>
          apply(deltas.reverse)

      // Where a source gives documents, a base record which gives one that is there without
      // it wants attention: the source has come to have a document of its name.
      def outright(origin: Point, lines: List[Text]): Rendition =
        if space.sourced.absent || rendition(parent).absent then Rendition(lines)()
        else Rendition(lines, issues = List(Issue.Overlay(origin)))()

      def apply(deltas: List[Point]): Optional[Rendition] =
        rendition(parent).let(_.content).let(render(deltas, _))

      def render(deltas: List[Point], reference: Content): Rendition =
        val placed: List[Placement | Issue] = deltas.bind: origin =>
          val hunks: List[Sealed] =
            folios.at(origin).let(_.only { case Folio.Delta(_, hunks) => hunks }).or(Nil)

          val source: Optional[Content] =
            if origin == point then reference
            else space.storageParent(origin).let(rendition(_)).let(_.content)

          source.lay(List[Placement | Issue]()): source =>
            // A hunk whose anchor is found has the lines of it from where it was found.
            val located: List[(Int, Hunk) | Issue] = hunks.map: hunk =>
              Rebase.locate(hunk, source) match
                case Content.Absent  => Issue.Orphan(origin, hunk)
                case Content.Several => Issue.Ambiguous(origin, hunk)
                case start           => (start, hunk.open(source, start))

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

  // Of hunks which have places, those which are applied, in the order of their places, and the
  // conflicts among them. Whether one is applied depends only on those it overlaps, so this may
  // be asked of any of them which overlap none of the rest.
  private def choose(candidates: List[Placement])
  :   (List[Placement], List[Issue]) raises Axis.Error =

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

    def rival(candidate: Placement): Optional[Placement] =
      val start = Integer.valueOf(candidate.start)
      val end = Integer.valueOf(candidate.end)

      def offer(best: Optional[(Int, Placement)], found: Optional[(Int, Placement)])
      :   Optional[(Int, Placement)] =

        found.lay(best): pair =>
          if pair(1).overlaps(candidate) && best.lay(true)(_(0) < pair(0)) then pair else best

      @tailrec
      def scan(values: java.util.Iterator[(Int, Placement)], best: Optional[(Int, Placement)])
      :   Optional[(Int, Placement)] =

        if !values.hasNext then best else scan(values, offer(best, values.next.nn))

      val preceding: Optional[(Int, Placement)] =
        Optional(spans.lowerEntry(start)).let(_.getValue.nn)

      val best: Optional[(Int, Placement)] =
        if candidate.start == candidate.end
        then offer(offer(Unset, Optional(points.get(start))), preceding)
        else
          val spanning = spans.subMap(start, true, end, false).nn.values.nn.iterator.nn
          val inserting = points.subMap(start, false, end, false).nn.values.nn.iterator.nn
          scan(inserting, scan(spanning, offer(Unset, preceding)))

      best.let(_(1))

    // Each candidate is accepted unless it has a rival, with which it may conflict.
    @tailrec
    def accept
      ( todo:      scala.List[Placement],
        accepted:  scala.List[Placement],
        conflicts: scala.List[Issue],
        number:    Int )
    :   (scala.List[Placement], scala.List[Issue]) =

      if todo.isEmpty then (accepted, conflicts) else
        val candidate = todo.head

        rival(candidate) match
          case rival: Placement =>
            val below = space.below(candidate.origin, rival.origin)
            val comparable = below || space.below(rival.origin, candidate.origin)

            if comparable || rival.fresh != candidate.fresh || rival.lines == candidate.lines
            then accept(todo.tail, accepted, conflicts, number)
            else accept(todo.tail, accepted, Issue.Conflict(rival, candidate) :: conflicts, number)

          case _ =>
            if candidate.start == candidate.end
            then points.put(Integer.valueOf(candidate.start), (number + 1, candidate))
            else spans.put(Integer.valueOf(candidate.start), (number + 1, candidate))

            accept(todo.tail, candidate :: accepted, conflicts, number + 1)

    val (accepted, conflicts) = accept(ranked.stdlib, scala.Nil, scala.Nil, 0)

    val ordered =
      List.from(accepted).order: placement =>
        placement.start*2 + (if placement.start == placement.end then 0 else 1)

    (ordered, List.from(conflicts.reverse))

  private def resolve(reference: Content, candidates: List[Placement], issues: List[Issue])
  :   Rendition raises Axis.Error =

    val (ordered, conflicts) = choose(candidates)
    val buffer = scala.collection.mutable.ListBuffer[Text]()
    val placed = scala.collection.mutable.ListBuffer[Placement]()

    val position = ordered.fold(0): (position, placement) =>
      buffer ++= reference.slice(position, placement.start).stdlib
      val from = buffer.length
      buffer ++= placement.lines.stdlib
      placed += placement.copy(from = from, to = buffer.length)
      placement.end

    buffer ++= reference.slice(position, reference.size).stdlib
    val lines = List.from(buffer.toList)
    val placements = List.from(placed.toList)
    val stale = placements.filter(!_.fresh).map(Issue.Stale(_))

    Rendition(lines, reference.lines, placements, issues + stale + conflicts)
      ( reference, candidates )

  def store(point: Point, lines: Optional[List[Text]]): Dossier raises Axis.Error =
    space.storageAxis(point).lay(retain(point, lines.let(Folio.Base(_)))): axis =>
      val bare = retain(point, Unset)

      def gone: Dossier =
        if bare.content(point).absent then bare else retain(point, Folio.Absent(axis.name))

      lines.lay(gone): lines =>
        val delta: Dossier = folios.at(point) match
          case Folio.Delta(_, _) => this
          case _                 => retain(point, Folio.Delta(axis.name, Nil))

        delta.rendition(point).lay(retain(point, Folio.Base(lines))): rendition =>
          val keep = axis.preference == Preference.Own
          val hunks = Inscription.hunks(rendition, lines, point, keep)

          def held(hunks: List[Hunk]): Dossier =
            retain(point, Folio.Delta(axis.name, hunks.map(_.seal)))

          val whole = held(hunks)
          val issues = whole.issues(point).size
          val pending = whole.pending(point).size

          // A hunk without which the content is the same, and no more of it wants attention,
          // says nothing an inherited one does not.
          def same(dossier: Dossier): Boolean =
            dossier.content(point) == lines && dossier.issues(point).size <= issues &&
              dossier.pending(point).size <= pending

          // Whether a hunk may go is decided, where it can be, from the hunks about it alone.
          // `Alone` is a fresh hunk which gives the lines it replaces and which overlaps no
          // other: without it, only its lines' being accounted for is lost. `Twin` is a hunk
          // which overlaps only an inherited hunk at the same lines, which overlaps only it:
          // without it, the inherited hunk is applied in its place. `Needed` is one which
          // overlaps nothing and changes its lines, and any other is `Tangled`, and is tried.
          enum Kind:
            case Needed, Tangled
            case Alone(pending: Int)
            case Twin(same: Boolean, issues: Int)

          val kinds: scala.collection.immutable.Map[Hunk, Kind] =
            val rendition = whole.rendition(point)
            val all: scala.Array[Placement] = rendition.let(_.candidates).or(Nil).stdlib.toArray
            val spans = all.filter: placement => placement.start < placement.end
            val starts: scala.Array[Int] = spans.map(_.start).sorted
            val ends: scala.Array[Int] = spans.map(_.end).sorted

            val gaps: scala.Array[Int] =
              all.filter { placement => placement.start == placement.end }.map(_.start).sorted

            // How many of the sorted numbers are less than `bound`.
            def below(sorted: scala.Array[Int], bound: Int): Int =
              Content.search(sorted.length)(sorted(_) >= bound)

            // How many other located hunks a placement overlaps.
            def overlaps(placement: Placement): Int =
              if placement.start == placement.end then
                below(starts, placement.start) - below(ends, placement.start + 1) +
                  below(gaps, placement.start + 1) - below(gaps, placement.start) - 1
              else
                below(starts, placement.end) - below(ends, placement.start + 1) - 1 +
                  below(gaps, placement.end) - below(gaps, placement.start + 1)

            val inherited: scala.collection.immutable.Map[(Int, Int), scala.Array[Placement]] =
              all.filter(_.origin != point).groupBy: placement => (placement.start, placement.end)

            val translated = translation(point)
            val reference = rendition.let(_.base)

            def unaccounted(placement: Placement): Int = reference.lay(0): reference =>
              @tailrec
              def tally(line: Int, count: Int): Int =
                if line >= placement.end then count
                else tally(line + 1, if reference(line) != t"" then count + 1 else count)

              tally(placement.start, 0)

            val own = all.filter(_.origin == point)

            val twice: scala.collection.immutable.Set[Hunk] =
              own.groupBy(_.hunk).filter(_(1).length > 1).keySet

            val pairs = own.iterator.map: placement =>
              val count = overlaps(placement)
              val none = scala.Array[Placement]()
              val twins = inherited.getOrElse((placement.start, placement.end), none)

              val kind: Kind =
                if twice.contains(placement.hunk) then Kind.Tangled
                else if count == 0 then
                  if placement.hunk.stale then Kind.Tangled
                  else if placement.hunk.replaced != placement.lines then Kind.Needed
                  else Kind.Alone(if translated then unaccounted(placement) else 0)
                else if count == 1 && twins.length == 1 && overlaps(twins(0)) == 1 then
                  Kind.Twin
                    ( twins(0).lines == placement.lines,
                      (if twins(0).fresh then 0 else 1) - (if placement.fresh then 0 else 1) )
                else
                  Kind.Tangled

              placement.hunk -> kind

            scala.collection.immutable.Map.from(pairs)

          // The kinds say what a hunk's going would change in content which is the lines: if
          // the hunks do not give the lines, two of them being at one place, each is tried.
          val sound: Boolean = whole.content(point) == lines

          // A tangled hunk is tried among the hunks it is tangled with, and not in the whole
          // document: the located hunks fall into groups, each of hunks which overlap one
          // another and no hunk of another group, and what a group shows over the lines it
          // spans does not depend on any other.
          val located: scala.Array[Placement] =
            whole.rendition(point).let(_.candidates).or(Nil).stdlib.toArray

          val base: Optional[Content] = whole.rendition(point).let(_.base)
          val translated: Boolean = translation(point)

          // For each located hunk, in the order of the places, the first of its group and the
          // line at which its group ends.
          val groups: scala.collection.immutable.Map[Hunk, (Int, Int, scala.Array[Int])] =
            val sorted: scala.Array[Int] = located.indices.toArray.sortBy: index =>
              located(index).start*2 + (if located(index).start == located(index).end then 0 else 1)

            @tailrec
            def gather
              ( index:   Int,
                members: scala.List[Int],
                low:     Int,
                reach:   Int,
                done:    scala.List[(Int, Int, scala.Array[Int])] )
            :   scala.List[(Int, Int, scala.Array[Int])] =

              def closed = if members.isEmpty then done else (low, reach, members.toArray) :: done

              if index == sorted.length then closed else
                val placement = located(sorted(index))
                val insertion = placement.start == placement.end

                val joins =
                  !members.isEmpty &&
                    (placement.start < reach || insertion && low == reach && placement.start == low)

                val next = sorted(index)

                if joins
                then gather(index + 1, next :: members, low, reach.max(placement.end), done)
                else gather(index + 1, scala.List(next), placement.start, placement.end, closed)

            val own = located.filter(_.origin == point).groupBy(_.hunk)

            scala.collection.immutable.Map.from:
              gather(0, scala.Nil, 0, 0, scala.Nil).iterator.flatMap: (low, reach, members) =>
                members.iterator.map(located(_)).filter(_.origin == point).flatMap: placement =>
                  if own(placement.hunk).length > 1 then scala.None
                  else scala.Some(placement.hunk -> (low, reach, members.sorted))

          // The lines which the hunks of one group give for the lines it spans, how many issues
          // are among them, and how many of those lines are pending.
          def shown(reference: Content, low: Int, reach: Int, members: List[Placement])
          :   (scala.List[Text], Int, Int) =

            val (applied, conflicts) = choose(members)
            val buffer = scala.collection.mutable.ListBuffer[Text]()
            val covered = new scala.Array[Boolean](reach - low)

            val position = applied.fold(low): (position, placement) =>
              buffer ++= reference.slice(position, placement.start).stdlib
              buffer ++= placement.lines.stdlib
              Content.ascend(placement.start, placement.end): line => covered(line - low) = true
              placement.end

            buffer ++= reference.slice(position, reach).stdlib

            val unplaced =
              if !translated then 0 else List.range(low, reach).count: line =>
                reference(line) != t"" && !covered(line - low)

            (buffer.toList, applied.count(!_.fresh) + conflicts.size, unplaced)

          // Whether a tangled hunk may go, of the hunks which `remains` holds of, and what
          // then becomes of the numbers of issues and of pending lines; or `Unset` if that
          // cannot be decided from its group.
          def locally(hunk: Hunk, remains: Hunk => Boolean, issues0: Int, pending0: Int)
          :   Optional[(Boolean, Int, Int)] =

            base.let: reference =>
              Optional(groups.get(hunk).orNull).let: (low, reach, members) =>
                val present = List.from:
                  members.iterator.map(located(_)).filter: placement =>
                    placement.origin != point || remains(placement.hunk)

                val absent =
                  present.filter: placement => placement.origin != point || placement.hunk != hunk

                val (lines1, issues1, pending1) = shown(reference, low, reach, present)
                val (lines2, issues2, pending2) = shown(reference, low, reach, absent)
                val fewer = issues0 - issues1 + issues2
                val unplaced = pending0 - pending1 + pending2
                (lines1 == lines2 && fewer <= issues && unplaced <= pending, fewer, unplaced)

          // Dropping one may leave another with nothing to do, so this repeats until none goes.
          def prune(hunks: List[Hunk], issues0: Int, pending0: Int): List[Hunk] =
            val gone = scala.collection.mutable.HashSet[Hunk]()
            val here = hunks.stdlib.toSet
            def left: List[Hunk] = if gone.isEmpty then hunks else hunks.filter(!gone.contains(_))

            val current = hunks.fold((issues0, pending0)): (current, hunk) =>
              val kind = if sound then kinds.getOrElse(hunk, Kind.Tangled) else Kind.Tangled

              if gone.contains(hunk) then current else kind match
                case Kind.Needed =>
                  current

                case Kind.Alone(more) =>
                  if current(1) + more > pending then current else
                    gone += hunk
                    (current(0), current(1) + more)

                case Kind.Twin(same, more) =>
                  if !same || current(0) + more > issues then current else
                    gone += hunk
                    (current(0) + more, current(1))

                case Kind.Tangled =>
                  // Where its group cannot say, the document is derived without the hunk.
                  def derived: (Boolean, Int, Int) =
                    val fewer = held(left.filter(_ != hunk))
                    (same(fewer), fewer.issues(point).size, fewer.pending(point).size)

                  val found: Optional[(Boolean, Int, Int)] =
                    if !sound then Unset
                    else locally(hunk, hunk => here.contains(hunk) && !gone.contains(hunk),
                        current(0), current(1))

                  val (goes, fewer, unplaced) = found.or(derived)

                  if !goes then current else
                    gone += hunk
                    (fewer, unplaced)

            if gone.isEmpty then hunks else prune(left, current(0), current(1))

          val needed = prune(hunks, issues, pending)

          // Whatever was decided, the hunks kept must still give the lines, with no more that
          // wants attention; if they do not, every hunk is kept.
          if needed.nil && same(bare) then bare else
            val pruned = held(needed)
            if same(pruned) then pruned else whole

  // What wants attention in the content at a point.
  def issues(point: Point): List[Issue] raises Axis.Error =
    rendition(point).let(_.issues).or(baseless(point))

  // Where a source gives documents, hunks which a point has or inherits want attention if the
  // document is absent there: the source no longer has what they were written against. They
  // are the hunks which would have been applied, from the most specific origin.
  private def baseless(point: Point): List[Issue] raises Axis.Error =
    def recur(todo: List[Point], found: List[Point]): List[Point] = todo match
      case origin :: tail => folios.at(origin) match
        case Folio.Delta(_, Nil)             => recur(tail, found)
        case Folio.Delta(_, _)               => recur(tail, origin :: found)
        case Folio.Base(_) | Folio.Absent(_) => found
        case _                               => recur(tail, found)

      case _ =>
        found

    if space.sourced.absent || space.source(point) then Nil
    else recur(specific(space.origins(point)), Nil).reverse.map(Issue.Baseless(_))

  // The dossier in which the stale hunks at `point` have been checked: each is stored there as
  // it is shown, against the lines its storage parent now has, and is stale no longer.
  // Only those which `select` chooses are checked; the others stay as they are.
  def confirm(point: Point, select: Placement => Boolean = placement => true)
  :   Dossier raises Axis.Error =

    space.storageAxis(point).lay(this): axis =>
      rendition(point).lay(this): rendition =>
        def chosen(placement: Placement): Boolean = !placement.fresh && select(placement)
        val stale = rendition.placements.filter(chosen)

        val checked: List[(Int, Hunk)] = stale.map: placement =>
          val hunk = Hunk(rendition.base.slice(placement.start, placement.end), placement.lines)
          (placement.start, Inscription.anchor(hunk, placement.start, rendition.base))

        val kept: List[(Int, Hunk)] =
          rendition.placements.filter: placement => placement.origin == point && !chosen(placement)
          . map: placement => (placement.start, placement.hunk)

        val hunks = (kept + checked).order { (start, hunk) => hunk.rank(start) }.map(_(1))
        if stale.nil then this else retain(point, Folio.Delta(axis.name, hunks.map(_.seal)))

  // Whether the point's content is a translation of its storage parent's.
  def translation(point: Point): Boolean raises Axis.Error =
    space.storageAxis(point).lay(false): axis => axis.value(point(axis)).translation

  // The lines of the storage parent's content, counted from nought, which a translation has yet
  // to replace: those which are not blank, and which no hunk takes the place of.
  def pending(point: Point): List[Int] raises Axis.Error =
    unplaced.get(point).getOrElse:
      val result = uncovered(point)
      unplaced(point) = result
      result

  private def uncovered(point: Point): List[Int] raises Axis.Error =
    if !translation(point) then Nil else rendition(point).lay(Nil): rendition =>
      val reference = rendition.base
      val covered = new scala.Array[Boolean](reference.size)

      rendition.placements.each: placement =>
        Content.ascend(placement.start, placement.end.min(covered.length))(covered(_) = true)

      val found = scala.collection.mutable.ListBuffer[Int]()

      Content.ascend(0, covered.length): index =>
        if reference(index) != t"" && !covered(index) then found += index

      List.from(found.toList)

  def affirm(point: Point, spans: List[(Int, Int)]): Dossier raises Axis.Error =
    space.storageAxis(point).lay(this): axis =>
      rendition(point).lay(this): rendition =>
        // The hunks in place are in order and apart, so the first of them to end after a span
        // begins is the only one which need be asked whether it meets the span: it does if it
        // begins, or inserts, before the span ends.
        val placed: scala.Array[Placement] = rendition.placements.stdlib.toArray

        val free = spans.filter: (start, end) =>
          val first = Content.search(placed.length)(placed(_).end > start)
          start < end && (first == placed.length || placed(first).start >= end)

        val held = folios.at(point) match
          case Folio.Base(_) | Folio.Absent(_) => true
          case _                               => false

        if free.nil || held || !translation(point) then this else
          val kept: List[(Int, Hunk)] =
            rendition.placements.filter(_.origin == point)
            . map: placement => (placement.start, placement.hunk)

          val added: List[(Int, Hunk)] = free.map: (start, end) =>
            val same = rendition.base.slice(start, end)
            (start, Inscription.anchor(Hunk(same, same), start, rendition.base))

          val hunks = (kept + added).order { (start, hunk) => hunk.rank(start) }.map(_(1))
          retain(point, Folio.Delta(axis.name, hunks.map(_.seal)))

  // The dossier in which a hunk stored at `point` which only takes out the lines `start` to
  // `end` of its storage parent's content, and one stored there which only puts lines in at
  // either end of them, are one hunk which replaces them: lines once left out of a translation,
  // and since translated. If that would change the content of any point, or what wants
  // attention there, they stay as they are.
  def unite(point: Point, spans: List[(Int, Int)]): Dossier raises Axis.Error =
    // Only the points at or below `point` have anything of what is stored there.
    val lower = space.points.filter(space.below(_, point))

    def same(united: Dossier, dossier: Dossier): Boolean =
      !lower.exists: other =>
        united.content(other) != dossier.content(other) ||
          united.issues(other).size != dossier.issues(other).size ||
          united.pending(other).size > dossier.pending(other).size

    def one(check: Boolean)(dossier: Dossier, span: (Int, Int)): Dossier =
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
                Hunk(rendition.base.slice(taking.start, taking.end), putting.hunk.lines)

              val hunks: List[(Int, Hunk)] =
                rendition.placements.filter: placement =>
                  placement.origin == point && placement != taking && placement != putting

                . map { placement => (placement.start, placement.hunk) } +
                  List((taking.start, Inscription.anchor(whole, taking.start, rendition.base)))

              val ordered = hunks.order { (start, hunk) => hunk.rank(start) }.map(_(1))
              val united = dossier.retain(point, Folio.Delta(axis.name, ordered.map(_.seal)))
              if !check || same(united, dossier) then united else dossier

    // Every span is united, and the points are derived once to see that nothing has changed;
    // only if something has is each span tried alone.
    val all = spans.fold(this)(one(false))

    if all.folios == folios then this
    else if same(all, this) then all
    else spans.fold(this)(one(true))

  // The dossier after a check-in of `lines` at `point`, in which only the points within `scope`
  // may differ in content: any other point below `point` whose content would change is pinned to
  // what it was. Hunks stored below `point` follow the change to their storage parents.
  def checkin(point: Point, lines: Optional[List[Text]], scope: Point => Boolean = point => true)
  :   Dossier raises Axis.Error raises Checkin.Error =

    if space.source(point) then abort(Checkin.Error(point, Checkin.Reason.Sourced))
    val every = space.points
    val below = every.filter: other => other != point && space.below(other, point)

    val result = below.fold(store(point, lines)): (dossier, other) =>
      val followed = dossier.follow(this, other)

      if scope(other) || followed.content(other) == content(other) then followed
      else followed.store(other, content(other))

    if result.content(point) != lines then abort(Checkin.Error(point, Checkin.Reason.Unequal))

    below.each: other =>
      if !scope(other) && result.content(other) != content(other)
      then abort(Checkin.Error(other, Checkin.Reason.Scope))

    every.each: other =>
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
          former.rendition(parent).let(_.content).lay(this): old =>
            rendition(parent).let(_.content).lay(this): next =>
              if old.lines == next.lines then this else
                val places: List[(Int, Sealed)] =
                  hunks.map: hunk => (Rebase.locate(hunk, old), hunk)

                val located: List[(Int, Hunk)] =
                  places.filter(_(0) >= 0).map: (start, hunk) => (start, hunk.open(old, start))

                val unlocated: List[Sealed] = places.filter(_(0) < 0).map(_(1))

                val moved: List[(Int, Hunk)] = Rebase(located, old, next).sweep:
                  case Rebase.Outcome.Fresh(start, hunk) =>
                    // An insertion must keep a line of context, or the document's start.
                    val anchored = !hunk.insertion || !hunk.before.nil || hunk.start

                    if anchored && Rebase.locate(hunk, next) == start then (start, hunk)
                    else (start, Inscription.anchor(hunk, start, next))

                  case Rebase.Outcome.Stale(start, _, hunk) =>
                    (start, Inscription.anchor(hunk, start, next))

                val ordered = moved.order { (start, hunk) => hunk.rank(start) }.map(_(1))
                retain(point, Folio.Delta(axis, ordered.map(_.seal) + unlocated))

        case _ =>
          this
