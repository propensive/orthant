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

import proscenium.{List, Nil, `::`}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.sortingAlgorithms.timsort

// The indirect view of a document at a point: its storage parent's content, as the reference,
// with the hunks which make the point's content of it. Every line begins with a sigil, which
// is followed by a space and the line's text, or by nothing for an empty line; a line with
// nothing on it at all is an empty line which the two share.
//
//   `  `  a line the two share            `- `  a line of the reference which is replaced
//   `? `  a line yet to be translated     `+ `  a line which replaces it, or is inserted
//   `! `  a line a stale hunk was written against, before the reference changed
//
// Lines marked `?` which `+` lines follow directly are translated by those, just as if they had
// been marked `-`.
object Indirect:
  enum Reason:
    case Sigil, Reference, Former

  case class Error(line: Int, reason: Reason)(using Diagnostics)
  extends fulminate.Error
    ( reason match
        case Reason.Reference =>
          m"""
            the reference's lines in the view, which are those marked - and those shared, differ
            near line $line from those checked out
          """

        case Reason.Former =>
          m"""
            the lines marked ! at line $line of the view were changed; they may only be deleted,
            together
          """

        case Reason.Sigil =>
          m"line $line of the view does not begin with a sigil and a space" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Sigil     => Orthant.bolt(4, 7)
      case Reason.Reference => Orthant.bolt(4, 8)
      case Reason.Former    => Orthant.bolt(4, 9)

  // A run of the view which is not shared: the lines `start` to `end` of the reference give
  // way to `lines`. `former` is its `!` lines, and `line` is where it begins in the view.
  case class Passage
    ( start:    Int,
      end:      Int,
      replaced: List[Text],
      lines:    List[Text],
      former:   List[Text],
      line:     Int,
      pending:  Boolean = false )

  // A line of the view which the reference and the content share.
  case class Shared(line: Text)

  private def mark(sigil: Text, line: Text): Text = if line == t"" then sigil else t"$sigil $line"

  // The reference with the hunks in place. `pending` marks the shared lines as yet to be
  // translated.
  def render(reference: List[Text], placements: List[Placement], pending: Boolean): List[Text] =
    render(Content(reference), placements, pending)

  def render(reference: Content, placements: List[Placement], pending: Boolean): List[Text] =
    val buffer = scala.collection.mutable.ListBuffer[Text]()

    def shared(from: Int, to: Int): Unit =
      var index = from

      while index < to do
        val line = reference(index)
        buffer += (if line == t"" then t"" else if pending then t"? $line" else t"  $line")
        index += 1

    var position = 0

    placements.each: placement =>
      val own = if placement.fresh then placement.lines else placement.hunk.lines
      val former = if placement.fresh then Nil else placement.hunk.former
      shared(position, placement.start)
      former.each { line => buffer += mark(t"!", line) }
      var index = placement.start

      while index < placement.end do
        buffer += mark(t"-", reference(index))
        index += 1

      own.each { line => buffer += mark(t"+", line) }
      position = placement.end

    shared(position, reference.size)
    List.from(buffer.toList)

  // The reference of a document's view at a point: its storage parent's content, or nothing
  // where the document is held in full, or the storage parent has none.
  def reference(dossier: Dossier, point: Point): List[Text] raises Axis.Error =
    dossier.rendition(point).let(_.reference).or:
      dossier.space.storageParent(point).let(dossier.content(_)).or(Nil)

  // The view of a document at a point, or `Unset` if it does not exist there.
  def view(dossier: Dossier, point: Point): Optional[List[Text]] raises Axis.Error =
    dossier.rendition(point).let: rendition =>
      if rendition.reference.nil && rendition.placements.nil then rendition.lines.map(mark(t"+", _))
      else render(rendition.base, rendition.placements, dossier.translation(point))

  // The view in which each `?` line that `+` lines follow, with nothing between but other `?`
  // and `-` lines, is a `-` line: what follows it is its translation.
  private def promote(view: List[Text]): List[Text] =
    view.reverse.fold((List[Text](), false)): (state, line) =>
      val (done, followed) = state

      line.keep(1) match
        case t"?" => ((if followed then t"-${line.skip(1)}" else line) :: done, followed)
        case t"-" => (line :: done, followed)
        case t"+" => (line :: done, true)
        case _    => (line :: done, false)

    . apply(0)

  // The shared lines and the passages of a view, in order.
  def parse(view: List[Text]): List[Shared | Passage] raises Error =
    def phase(sigil: Text): Int = sigil match
      case t"!" => 1
      case t"-" => 2
      case t"+" => 3
      case _    => 0

    // The lines of the passage which is open are gathered last first, and put in order when
    // it closes.
    def close(part: Shared | Passage): Shared | Passage = part match
      case passage: Passage =>
        passage.copy
          ( former = passage.former.reverse, replaced = passage.replaced.reverse,
            lines = passage.lines.reverse )

      case shared =>
        shared

    val done = scala.collection.mutable.ListBuffer[Shared | Passage]()
    var current: Optional[Passage] = Unset
    var position = 0
    var last = 0
    var number = 1

    promote(view).each: line =>
      val sigil = line.keep(1)
      val valid = line.length < 2 || line.keep(2).skip(1) == t" "
      val known = sigil == t"" || sigil == t" " || sigil == t"?" || phase(sigil) > 0
      if !valid || !known || line == t" " then abort(Error(number, Reason.Sigil))
      val text = line.skip(2)
      val next = phase(sigil)

      // A passage goes on while its lines come in the order `!`, `-`, `+`.
      val open: Optional[Passage] = if last > 0 && next >= last then current else Unset

      if open.absent || next == 0 then
        current.let { passage => done += close(passage) }
        current = Unset

      val passage = open.or(Passage(position, position, Nil, Nil, Nil, number))

      next match
        case 1 => current = passage.copy(former = text :: passage.former)
        case 2 => current = passage.copy(end = position + 1, replaced = text :: passage.replaced)
        case 3 => current = passage.copy(lines = text :: passage.lines)
        case _ => done += Shared(text)

      if next == 0 || next == 2 then position += 1
      last = next
      number += 1

    current.let { passage => done += close(passage) }
    List.from(done.toList)

  // What a check-in of an edited view asks for: the stale hunks whose `!` lines were deleted,
  // the content, and the runs of the reference which passages replace.
  case class Request(checked: List[Placement], lines: List[Text], spans: List[(Int, Int)])

  // Reads an edited view at `point`. The reference's lines in it must be those of the reference.
  def read(dossier: Dossier, point: Point, view: List[Text])
  :   Request raises Error raises Axis.Error =

    val parts = parse(view)
    val passages = parts.sweep { case passage: Passage => passage }
    val expected = reference(dossier, point)

    val found = parts.bind:
      case passage: Passage => passage.replaced
      case Shared(line)     => List(line)

    if found != expected then
      val agreed =
        Region.between(expected, found).head.let: region => if region.changed then 0 else region.to

      abort(Error(agreed.or(0) + 1, Reason.Reference))

    val stale = dossier.rendition(point).let(_.placements.filter(!_.fresh)).or(Nil)

    // The passage in which a stale hunk is as it was shown, `!` lines and all.
    val shown = scala.collection.mutable.HashMap[(Int, Int, List[Text]), Passage]()

    passages.each: passage =>
      shown.getOrElseUpdate((passage.start, passage.end, passage.former), passage)

    def kept(placement: Placement): Optional[Passage] =
      shown.get((placement.start, placement.end, placement.hunk.former)).getOrElse(Unset)

    // The stale hunks which each passage shows, as it was.
    val showing = scala.collection.mutable.HashMap[Passage, scala.collection.mutable.ListBuffer[Placement]]()

    stale.each: placement =>
      kept(placement).let: passage =>
        showing.getOrElseUpdate(passage, scala.collection.mutable.ListBuffer()) += placement

    passages.each: passage =>
      if !passage.former.nil && !showing.contains(passage)
      then abort(Error(passage.line, Reason.Former))

    // A stale passage which was left alone gives what it showed before, which need not be its
    // own lines; any other passage gives the lines it has.
    val lines = parts.bind:
      case Shared(line) =>
        List(line)

      case passage: Passage =>
        showing.get(passage).flatMap(_.find(_.hunk.lines == passage.lines)).map(_.lines)
        . getOrElse(passage.lines)

    Request
      ( stale.filter(kept(_).absent), lines,
        passages.filter(!_.replaced.nil).map { passage => (passage.start, passage.end) } )

  // The dossier in which the stale hunks a request names are checked.
  def confirm(dossier: Dossier, point: Point, request: Request): Dossier raises Axis.Error =
    def named(placement: Placement): Boolean = request.checked.exists: other =>
      other.start == placement.start && other.end == placement.end

    if request.checked.nil then dossier else dossier.confirm(point, named(_))

  // The dossier after a check-in of an edited view at `point`. A stale passage whose `!` lines
  // were deleted is checked, and is stale no longer; a passage which replaces lines by the same
  // lines, on a translation edge, is kept, and those lines are no longer to be translated.
  // Lines which were left out, and are now replaced, are replaced by one hunk.
  def checkin
    ( dossier: Dossier, point: Point, view: List[Text], scope: Point => Boolean = point => true )
  :   Dossier raises Error raises Axis.Error raises Checkin.Error =

    val request = read(dossier, point, view)
    confirm(dossier, point, request).checkin(point, request.lines, scope)
    . affirm(point, request.spans)
    . unite(point, request.spans)

  // ── Views against a change ──────────────────────────────────────────────────────────────────
  //
  // The view of a document at a point beside a change to its reference: `former` is what the
  // reference was, at an earlier value of some axis. Where the change touches nothing, the
  // view is the point's own content, as shared lines. Where it does, a passage shows the lines
  // the reference had, as `!` lines, the lines it has now, as `-` lines, and the point's
    // content there, as `+` lines. A stale hunk is shown likewise, whatever made it stale. Lines of
  // a translation's reference which the change brought, and which nothing yet translates, are
  // shown as `?` lines, as they are beside the reference.

  def alteration(dossier: Dossier, point: Point, former: List[Text])
  :   Optional[List[Shared | Passage]] raises Axis.Error =

    dossier.rendition(point).let: rendition =>
      val reference = rendition.base
      val placements: scala.Array[Placement] = rendition.placements.stdlib.toArray
      val regions: scala.Array[Region] = Region.between(former, reference.lines).stdlib.toArray
      val changed: scala.Array[Region] = regions.filter(_.changed)
      val formerLines = Content(former)

      def touches(hull: (Int, Int), placement: Placement): Boolean =
        val (from, to) = hull
        val (start, end) = (placement.start, placement.end)

        if from < to && start < end then from < end && start < to
        else if start == end then from <= start && start <= to
        else start < from && from < end

      // The least index of a sorted array whose value is at least `bound`.
      def least(values: scala.Array[Int], bound: Int): Int =
        var low = 0
        var high = values.length

        while low < high do
          val middle = (low + high) >>> 1
          if values(middle) < bound then low = middle + 1 else high = middle

        low

      def meet(last: (Int, Int), next: (Int, Int)): Boolean =
        next(0) < last(1) || (next(0) == last(1) && (next(0) == next(1) || last(0) == last(1)))

      def merge(todo: List[(Int, Int)], done: List[(Int, Int)]): List[(Int, Int)] = todo match
        case next :: tail => done match
          case last :: rest if meet(last, next) =>
            merge(tail, (last(0).min(next(0)), last(1).max(next(1))) :: rest)

          case _ =>
            merge(tail, next :: done)

        case _ =>
          done.reverse

      // The hunks of a rendition do not overlap, so the only hunk which can carry a hull beyond
      // one of its ends is the one which spans that end, if any: the last which is not empty
      // and begins before it.
      val solid: scala.Array[Placement] = placements.filter { placement => placement.start < placement.end }
      val solidStarts: scala.Array[Int] = solid.map(_.start)

      def spanning(boundary: Int): Optional[Placement] =
        val index = least(solidStarts, boundary) - 1
        if index >= 0 && boundary < solid(index).end then solid(index) else Unset

      def grow(hull: (Int, Int)): (Int, Int) =
        val from = spanning(hull(0)).let(_.start).or(hull(0)).min(hull(0))
        val to = spanning(hull(1)).let(_.end).or(hull(1)).max(hull(1))
        if (from, to) == hull then hull else grow((from, to))

      // A hull grows to take in every hunk it touches, and hulls which meet are one.
      def pass(hulls: List[(Int, Int)]): List[(Int, Int)] =
        var done: scala.List[(Int, Int)] = scala.Nil

        hulls.each: seed =>
          var hull = grow(seed)
          var joining = true

          while joining && !done.isEmpty do
            val last = done.head
            val (first, second) = if last(0) <= hull(0) then (last, hull) else (hull, last)

            if meet(first, second) then
              hull = grow((first(0).min(second(0)), first(1).max(second(1))))
              done = done.tail
            else joining = false

          done = hull :: done

        merge(List.from(done.reverse).order(_(0)), Nil)

      def settle(hulls: List[(Int, Int)]): List[(Int, Int)] =
        val next = pass(hulls)
        if next == hulls then hulls else settle(next)

      // What wants looking at: a stale hunk, and lines of the reference which changed and
      // which no fresh hunk wholly replaces. Changed lines which one does have been seen to.
      val fresh: scala.Array[Placement] = placements.filter(_.fresh).sortBy(_.start)
      val freshStarts: scala.Array[Int] = fresh.map(_.start)
      val reach: scala.Array[Int] = fresh.map(_.end).scanLeft(Int.MinValue)(_.max(_))

      def settled(from: Int, to: Int): Boolean = reach(least(freshStarts, from + 1)) >= to

      // Blank lines at either end of what changed want nothing.
      def trim(from: Int, to: Int): (Int, Int) =
        if from < to && reference(from) == t"" then trim(from + 1, to)
        else if from < to && reference(to - 1) == t"" then trim(from, to - 1)
        else (from, to)

      val seeds =
        List.from(changed.map { region => trim(region.from2, region.to2) }.iterator)
        . filter { (from, to) => from < to && !settled(from, to) } +
          List.from(placements.filter(!_.fresh).map { placement => (placement.start, placement.end) }.iterator)

      val hulls: scala.Array[(Int, Int)] = settle(merge(seeds.order(_(0)), Nil)).stdlib.toArray
      val hullFroms: scala.Array[Int] = hulls.map(_(0))
      val hullTos: scala.Array[Int] = hulls.map(_(1))

      // The first hull a hunk touches: none before the first which reaches its start can.
      def hullIndex(placement: Placement): Int =
        var index = least(hullTos, placement.start)
        var found = -1

        while found < 0 && index < hulls.length && hulls(index)(0) <= placement.end do
          if touches(hulls(index), placement) then found = index
          index += 1

        found

      val owners: scala.Array[Int] = placements.map(hullIndex)

      def hullAt(line: Int): Int =
        val index = least(hullFroms, line + 1) - 1
        if index >= 0 && line < hulls(index)(1) then index else -1

      // Each line of the content, keyed by where it is in the reference, with its hull's index.
      val keys = scala.collection.mutable.ArrayBuffer[Int]()
      val texts = scala.collection.mutable.ArrayBuffer[Text]()
      val owned = scala.collection.mutable.ArrayBuffer[Int]()

      def item(key: Int, line: Text, hull: Int): Unit =
        keys += key
        texts += line
        owned += hull

      var position = 0

      placements.indices.foreach: index =>
        val placement = placements(index)

        while position < placement.start do
          item(position, reference(position), hullAt(position))
          position += 1

        placement.lines.each(item(placement.start, _, owners(index)))
        position = placement.end

      while position < reference.size do
        item(position, reference(position), hullAt(position))
        position += 1

      val shown: scala.Array[scala.collection.mutable.ListBuffer[Text]] =
        scala.Array.fill(hulls.length)(scala.collection.mutable.ListBuffer[Text]())

      val loose = scala.collection.mutable.ArrayBuffer[Int]()

      keys.indices.foreach: index =>
        if owned(index) >= 0 then shown(owned(index)) += texts(index) else loose += index

      val looseKeys: scala.Array[Int] = loose.map(keys(_)).toArray

      val staleOf: scala.Array[scala.collection.mutable.ListBuffer[Text]] =
        scala.Array.fill(hulls.length)(scala.collection.mutable.ListBuffer[Text]())

      placements.indices.foreach: index =>
        val placement = placements(index)
        if !placement.fresh && owners(index) >= 0 then staleOf(owners(index)) ++= placement.hunk.former.stdlib

      val regionTos: scala.Array[Int] = regions.map(_.to2)
      val changedTos: scala.Array[Int] = changed.map(_.to2)
      val placementReach: scala.Array[Int] = placements.map(_.end).scanLeft(Int.MinValue)(_.max(_))

      // Where a boundary of the reference was in the former reference.
      def back(boundary: Int): List[Int] =
        val found = scala.collection.mutable.ListBuffer[Int]()
        var index = least(regionTos, boundary)

        while index < regions.length && regions(index).from2 <= boundary do
          val region = regions(index)

          if !region.changed then found += region.from + boundary - region.from2
          else if region.from2 < boundary && boundary < region.to2
          then { found += region.from; found += region.to }
          else
            if boundary == region.from2 then found += region.from
            if boundary == region.to2 then found += region.to

          index += 1

        List.from(found)

      def passage(index: Int): Passage =
        val hull = hulls(index)
        val (from, to) = hull

        var altered = false
        var next = least(changedTos, from)

        while !altered && next < changed.length && changed(next).from2 <= to do
          val region = changed(next)

          altered =
            if region.from2 == region.to2 then from <= region.from2 && region.from2 <= to
            else region.from2 < to && from < region.to2

          next += 1

        // Lines of a translation's reference which no hunk touches are yet to be translated.
        // A hunk which touches the hull ends no earlier than its start.
        var touched = false
        var candidate = (least(placementReach, from) - 1).max(0)

        while !touched && candidate < placements.length && placements(candidate).start <= to do
          touched = touches(hull, placements(candidate))
          candidate += 1

        val wanting = dossier.translation(point) && !touched

        val before: List[Text] =
          if altered
          then formerLines.slice(back(from).fold(formerLines.size)(_.min(_)), back(to).fold(0)(_.max(_)))
          else List.from(staleOf(index))

        Passage
          ( from, to, reference.slice(from, to), List.from(shown(index)),
            if wanting then Nil else before, 0, wanting )

      // The lines no hull holds from `from` up to `to`.
      def shared(from: Int, to: Optional[Int]): List[Shared] =
        val first = least(looseKeys, from)
        val last = to.lay(looseKeys.length)(least(looseKeys, _))
        List.from((first until last.max(first)).map { index => Shared(texts(loose(index))) })

      val parts = scala.collection.mutable.ListBuffer[Shared | Passage]()
      var at = 0

      hulls.indices.foreach: index =>
        parts ++= shared(at, hulls(index)(0)).stdlib
        parts += passage(index)
        at = hulls(index)(1)

      parts ++= shared(at, Unset).stdlib
      List.from(parts)

  def alterationView(dossier: Dossier, point: Point, former: List[Text])
  :   Optional[List[Text]] raises Axis.Error =

    alteration(dossier, point, former).let: parts =>
      parts.bind:
        case Shared(line) =>
          List(if line == t"" then t"" else t"  $line")

        case passage: Passage if passage.pending =>
          passage.replaced.map: line => if line == t"" then t"" else t"? $line"

        case passage: Passage =>
          passage.former.map(mark(t"!", _)) + passage.replaced.map(mark(t"-", _)) +
            passage.lines.map(mark(t"+", _))

  // Reads an edited view against a change. The shared and `+` lines are the content. The
  // passages must be those which were shown, by their `-` lines, in order; a passage's `!`
  // lines may be deleted, all of them, which checks whatever is stale in it. A passage of `?`
  // lines is one only where it has been translated.
  def alterationRead(dossier: Dossier, point: Point, former: List[Text], view: List[Text])
  :   Request raises Error raises Axis.Error =

    val parts = parse(view)

    val shown =
      alteration(dossier, point, former).or(Nil).sweep { case passage: Passage => passage }

    val edited = parts.sweep:
      case passage: Passage if !passage.replaced.nil || !passage.former.nil => passage

    def solid(lines: List[Text]): List[Text] = lines.filter(_ != t"")

    def numbered(passages: List[Passage]): List[(Passage, Int)] =
      passages.zip(List.range(0, passages.size))

    // The passages are matched by their `-` lines which are not blank, so that one passage may
    // be made into several, a paragraph each. Those with no such line are matched in order.
    val anchored = numbered(shown.filter { passage => !solid(passage.replaced).nil })
    val loose = shown.filter: passage => solid(passage.replaced).nil
    val typed = numbered(edited.filter { passage => !solid(passage.replaced).nil })
    val adrift = edited.filter: passage => solid(passage.replaced).nil

    val expected: List[(Text, Int, Boolean)] = anchored.bind: (passage, index) =>
      solid(passage.replaced).map((_, index, passage.pending))

    val found: List[(Text, Int)] =
      typed.bind: (passage, index) => solid(passage.replaced).map((_, index))

    // Each line found with the line expected which it is: they come in the same order, and
    // only the lines of a passage yet to be translated may be missing. Where a line may be
    // matched or passed over, it is matched if the rest can be. Whether the rest can be, from
    // each pair of positions, is remembered, so no pair is tried twice.
    val wanted: scala.Array[(Text, Int, Boolean)] = expected.stdlib.toArray
    val seen: scala.Array[(Text, Int)] = found.stdlib.toArray
    val known = java.util.HashMap[java.lang.Long, java.lang.Boolean]()

    def key(index: Int, number: Int): java.lang.Long =
      java.lang.Long.valueOf(index.toLong*(seen.length + 1) + number).nn

    def matching(index: Int, number: Int): Boolean =
      number < seen.length && seen(number)(0) == wanted(index)(0)

    // Whether the lines found from `number` on are the lines expected from `index` on.
    def feasible(index0: Int, number0: Int): Boolean =
      val stack = scala.collection.mutable.Stack[(Int, Int)]((index0, number0))

      while !stack.isEmpty do
        val (index, number) = stack.top

        if index == wanted.length then
          known.put(key(index, number), java.lang.Boolean.valueOf(number == seen.length))
          stack.pop()
        else if known.containsKey(key(index, number)) then stack.pop()
        else
          val taking = matching(index, number)
          val taken = if taking then known.get(key(index + 1, number + 1)) else null
          val optional = wanted(index)(2)

          if taking && taken == null then stack.push((index + 1, number + 1))
          else if taking && taken.nn.booleanValue then
            known.put(key(index, number), java.lang.Boolean.TRUE)
            stack.pop()
          else if !optional then
            known.put(key(index, number), java.lang.Boolean.FALSE)
            stack.pop()
          else
            val passed = known.get(key(index + 1, number))
            if passed == null then stack.push((index + 1, number)) else
              known.put(key(index, number), passed)
              stack.pop()

      known.get(key(index0, number0)).nn.booleanValue

    def align: Optional[List[(Int, Int)]] =
      if !feasible(0, 0) then Unset else
        val result = scala.collection.mutable.ListBuffer[(Int, Int)]()
        var index = 0
        var number = 0

        while index < wanted.length do
          if matching(index, number) && feasible(index + 1, number + 1) then
            result += ((wanted(index)(1), seen(number)(1)))
            number += 1

          index += 1

        List.from(result)

    val pairs: List[(Int, Int)] = align.or:
      val lines = scala.collection.immutable.Set.from(expected.stdlib.iterator.map(_(0)))

      val strange = typed.filter: (passage, number) =>
        solid(passage.replaced).exists: line => !lines.contains(line)

      val line = strange.head.let(_(0).line).or(edited.head.let(_.line).or(view.size))
      abort(Error(line, Reason.Reference))

    val typedPassages: scala.Array[Passage] = typed.stdlib.map(_(0)).toArray
    val covers = scala.collection.mutable.HashMap[Int, scala.collection.mutable.TreeSet[Int]]()
    val counts = scala.collection.mutable.HashMap[Int, Int]()

    pairs.each: (index, number) =>
      covers.getOrElseUpdate(index, scala.collection.mutable.TreeSet[Int]()) += number
      counts(index) = counts.getOrElse(index, 0) + 1

    def covering(index: Int): List[Passage] =
      List.from(covers.get(index).fold(scala.Nil)(_.toList).map(typedPassages(_)))

    // The spans of the passages whose `!` lines are gone.
    val settled: List[(Int, Int)] = anchored.bind: (passage, index) =>
      val former = covering(index).bind(_.former)

      if former.nil then (if passage.former.nil then Nil else List((passage.start, passage.end)))
      else if former == passage.former then Nil
      else abort(Error(covering(index).head.let(_.line).or(view.size), Reason.Former))

    def unanchored(shown: List[Passage], edited: List[Passage]): List[(Int, Int)] =
      val remaining: scala.Array[Passage] = edited.stdlib.toArray
      val spans = scala.collection.mutable.ListBuffer[(Int, Int)]()
      var next = 0

      shown.each: passage =>
        if next < remaining.length && remaining(next).former == passage.former then next += 1
        else spans += ((passage.start, passage.end))

      if next < remaining.length then abort(Error(remaining(next).line, Reason.Former))
      List.from(spans)

    val spans = settled + unanchored(loose, adrift)

    val lines = parts.bind:
      case Shared(line)     => List(line)
      case passage: Passage => passage.lines

    val stale = dossier.rendition(point).let(_.placements.filter(!_.fresh)).or(Nil)

    // A stale hunk is within a span if one begins no later and ends no earlier than it does.
    val ordered: scala.Array[(Int, Int)] = spans.stdlib.toArray.sortBy(_(0))
    val starts: scala.Array[Int] = ordered.map(_(0))
    val reach: scala.Array[Int] = ordered.map(_(1)).scanLeft(Int.MinValue)(_.max(_))

    def upto(bound: Int): Int =
      var low = 0
      var high = starts.length

      while low < high do
        val middle = (low + high) >>> 1
        if starts(middle) <= bound then low = middle + 1 else high = middle

      low

    val within = stale.filter: placement => reach(upto(placement.start)) >= placement.end

    // A passage yet to be translated, every line of which is replaced by itself, is translated
    // by the same lines.
    val same: List[(Int, Int)] = anchored.bind: (passage, index) =>
      val whole = counts.getOrElse(index, 0) == solid(passage.replaced).size
      val kept = !covering(index).exists: other => other.lines != other.replaced
      if passage.pending && whole && kept then List((passage.start, passage.end)) else Nil

    Request(within, lines, same)
