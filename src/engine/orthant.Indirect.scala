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
    def shared(lines: List[Text]): List[Text] = lines.map: line =>
      if line == t"" then t"" else if pending then t"? $line" else t"  $line"

    def recur(todo: List[Placement], position: Int): List[Text] = todo match
      case placement :: tail =>
        val own = if placement.fresh then placement.lines else placement.hunk.lines
        val former = if placement.fresh then Nil else placement.hunk.former

        shared(reference.excerpt(position, placement.start)) + former.map(mark(t"!", _)) +
          reference.excerpt(placement.start, placement.end).map(mark(t"-", _)) +
          own.map(mark(t"+", _)) + recur(tail, placement.end)

      case _ =>
        shared(reference.excerpt(position, reference.size))

    recur(placements, 0)

  // The reference of a document's view at a point: its storage parent's content, or nothing
  // where the document is held in full, or the storage parent has none.
  def reference(dossier: Dossier, point: Point): List[Text] raises Axis.Error =
    dossier.rendition(point).let(_.reference).or:
      dossier.space.storageParent(point).let(dossier.content(_)).or(Nil)

  // The view of a document at a point, or `Unset` if it does not exist there.
  def view(dossier: Dossier, point: Point): Optional[List[Text]] raises Axis.Error =
    dossier.rendition(point).let: rendition =>
      if rendition.reference.nil && rendition.placements.nil then rendition.lines.map(mark(t"+", _))
      else render(rendition.reference, rendition.placements, dossier.translation(point))

  private case class Reading
    ( parts: List[Shared | Passage] = Nil, position: Int = 0, phase: Int = 0, number: Int = 1 )

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

    promote(view).fold(Reading()): (reading, line) =>
      val sigil = line.keep(1)
      val valid = line.length < 2 || line.keep(2).skip(1) == t" "
      val known = sigil == t"" || sigil == t" " || sigil == t"?" || phase(sigil) > 0
      if !valid || !known || line == t" " then abort(Error(reading.number, Reason.Sigil))
      val text = line.skip(2)
      val next = phase(sigil)
      val position = reading.position

      // A passage goes on while its lines come in the order `!`, `-`, `+`.
      val open: Optional[Passage] = reading.parts.head.let:
        case passage: Passage if reading.phase > 0 && next >= reading.phase => passage
        case _                                                              => Unset

      val rest =
        if open.present then reading.parts.excerpt(1, reading.parts.size) else reading.parts

      val passage = open.or(Passage(position, position, Nil, Nil, Nil, reading.number))

      val part: Shared | Passage = next match
        case 1 => passage.copy(former = passage.former + List(text))
        case 2 => passage.copy(end = position + 1, replaced = passage.replaced + List(text))
        case 3 => passage.copy(lines = passage.lines + List(text))
        case _ => Shared(text)

      val moved = if next == 0 || next == 2 then 1 else 0
      val parts = if next == 0 then part :: reading.parts else part :: rest
      Reading(parts, position + moved, next, reading.number + 1)

    . parts.reverse

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
    def kept(placement: Placement): Optional[Passage] =
      passages.filter: passage =>
        passage.start == placement.start && passage.end == placement.end &&
          passage.former == placement.hunk.former

      . head

    passages.each: passage =>
      if !passage.former.nil && !stale.exists(kept(_) == passage)
      then abort(Error(passage.line, Reason.Former))

    // A stale passage which was left alone gives what it showed before, which need not be its
    // own lines; any other passage gives the lines it has.
    val lines = parts.bind:
      case Shared(line) =>
        List(line)

      case passage: Passage =>
        stale.filter: placement =>
          kept(placement) == passage && placement.hunk.lines == passage.lines

        . head.lay(passage.lines)(_.lines)

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

  private case class Item(key: Int, line: Text, hull: Optional[(Int, Int)])

  def alteration(dossier: Dossier, point: Point, former: List[Text])
  :   Optional[List[Shared | Passage]] raises Axis.Error =

    dossier.rendition(point).let: rendition =>
      val reference = rendition.reference
      val placements = rendition.placements
      val regions = Region.between(former, reference)
      val changed = regions.filter(_.changed)

      def touches(hull: (Int, Int), placement: Placement): Boolean =
        val (from, to) = hull
        val (start, end) = (placement.start, placement.end)

        if from < to && start < end then from < end && start < to
        else if start == end then from <= start && start <= to
        else start < from && from < end

      def merge(todo: List[(Int, Int)], done: List[(Int, Int)]): List[(Int, Int)] = todo match
        case next :: tail => done match
          case last :: rest
          if next(0) < last(1) ||
            (next(0) == last(1) && (next(0) == next(1) || last(0) == last(1))) =>
            merge(tail, (last(0).min(next(0)), last(1).max(next(1))) :: rest)

          case _ =>
            merge(tail, next :: done)

        case _ =>
          done.reverse

      // A hull grows to take in every hunk it touches, and hulls which meet are one.
      def settle(hulls: List[(Int, Int)]): List[(Int, Int)] =
        val grown = hulls.map: hull =>
          placements.filter(touches(hull, _)).fold(hull): (hull, placement) =>
            (hull(0).min(placement.start), hull(1).max(placement.end))

        val merged = merge(grown.order(_(0)), Nil)
        if merged == hulls then hulls else settle(merged)

      // What wants looking at: a stale hunk, and lines of the reference which changed and
      // which no fresh hunk wholly replaces. Changed lines which one does have been seen to.
      def settled(from: Int, to: Int): Boolean = placements.exists: placement =>
        placement.fresh && placement.start <= from && to <= placement.end

      // Blank lines at either end of what changed want nothing.
      def trim(from: Int, to: Int): (Int, Int) =
        if from < to && reference.excerpt(from, from + 1) == List(t"") then trim(from + 1, to)
        else if from < to && reference.excerpt(to - 1, to) == List(t"") then trim(from, to - 1)
        else (from, to)

      val seeds =
        changed.map: region => trim(region.from2, region.to2)
        . filter { (from, to) => from < to && !settled(from, to) } +
          placements.filter(!_.fresh).map: placement => (placement.start, placement.end)

      val hulls = settle(merge(seeds.order(_(0)), Nil))

      def hullOf(placement: Placement): Optional[(Int, Int)] =
        hulls.filter(touches(_, placement)).head

      def hullAt(line: Int): Optional[(Int, Int)] =
        hulls.filter { hull => hull(0) <= line && line < hull(1) }.head

      def lines(from: Int, to: Int): List[Item] =
        reference.excerpt(from, to).fold((from, List[Item]())): (state, line) =>
          (state(0) + 1, Item(state(0), line, hullAt(state(0))) :: state(1))

        . apply(1).reverse

      def walk(todo: List[Placement], position: Int): List[Item] = todo match
        case placement :: tail =>
          lines(position, placement.start) +
            placement.lines.map(Item(placement.start, _, hullOf(placement))) +
            walk(tail, placement.end)

        case _ =>
          lines(position, reference.size)

      val items = walk(placements, 0)

      // Where a boundary of the reference was in the former reference.
      def back(boundary: Int): List[Int] =
        def about(region: Region): Boolean = region.from2 <= boundary && boundary <= region.to2

        regions.filter(about(_)).bind: region =>
          if !region.changed then List(region.from + boundary - region.from2)
          else if region.from2 < boundary && boundary < region.to2 then List(region.from, region.to)
          else
            (if boundary == region.from2 then List(region.from) else Nil) +
              (if boundary == region.to2 then List(region.to) else Nil)

      def passage(hull: (Int, Int)): Passage =
        val (from, to) = hull

        val altered = changed.exists: region =>
          if region.from2 == region.to2 then from <= region.from2 && region.from2 <= to
          else region.from2 < to && from < region.to2

        // Lines of a translation's reference which no hunk touches are yet to be translated.
        val wanting = dossier.translation(point) && !placements.exists(touches(hull, _))

        val before: List[Text] =
          if altered
          then former.excerpt(back(from).fold(former.size)(_.min(_)), back(to).fold(0)(_.max(_)))
          else
            placements.filter: placement => !placement.fresh && hullOf(placement) == hull
            . bind(_.hunk.former)

        Passage
          ( from, to, reference.excerpt(from, to), items.filter(_.hull == hull).map(_.line),
            if wanting then Nil else before, 0, wanting )

      def shared(from: Int, to: Optional[Int]): List[Shared] =
        items.filter: item => item.hull.absent && from <= item.key && to.lay(true)(item.key < _)
        . map: item => Shared(item.line)

      def recur(todo: List[(Int, Int)], position: Int): List[Shared | Passage] = todo match
        case hull :: tail => shared(position, hull(0)) + List(passage(hull)) + recur(tail, hull(1))
        case _            => shared(position, Unset)

      recur(hulls, 0)

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
    // only the lines of a passage yet to be translated may be missing.
    def align(expected: List[(Text, Int, Boolean)], found: List[(Text, Int)])
    :   Optional[List[(Int, Int)]] =

      expected match
        case (line, index, optional) :: rest =>
          val taken: Optional[List[(Int, Int)]] = found match
            case (text, number) :: tail if text == line =>
              align(rest, tail).let((index, number) :: _)

            case _                                      => Unset

          taken.or(if optional then align(rest, found) else Unset)

        case _ =>
          if found.nil then Nil else Unset

    val pairs: List[(Int, Int)] = align(expected, found).or:
      val strange = typed.filter: (passage, number) =>
        solid(passage.replaced).exists: line => !expected.exists(_(0) == line)

      val line = strange.head.let(_(0).line).or(edited.head.let(_.line).or(view.size))
      abort(Error(line, Reason.Reference))

    def covering(index: Int): List[Passage] =
      val over = pairs.filter(_(0) == index).map(_(1)).distinct
      typed.filter { (other, number) => over.has(number) }.map(_(0))

    // The spans of the passages whose `!` lines are gone.
    val settled: List[(Int, Int)] = anchored.bind: (passage, index) =>
      val former = covering(index).bind(_.former)

      if former.nil then (if passage.former.nil then Nil else List((passage.start, passage.end)))
      else if former == passage.former then Nil
      else abort(Error(covering(index).head.let(_.line).or(view.size), Reason.Former))

    def unanchored(shown: List[Passage], edited: List[Passage]): List[(Int, Int)] = shown match
      case passage :: tail =>
        if edited.head.let(_.former == passage.former).or(false)
        then unanchored(tail, edited.excerpt(1, edited.size))
        else (passage.start, passage.end) :: unanchored(tail, edited)

      case _ =>
        edited.head.let: passage => abort(Error(passage.line, Reason.Former))
        Nil

    val spans = settled + unanchored(loose, adrift)

    val lines = parts.bind:
      case Shared(line)     => List(line)
      case passage: Passage => passage.lines

    val stale = dossier.rendition(point).let(_.placements.filter(!_.fresh)).or(Nil)

    val within = stale.filter: placement =>
      spans.exists: (from, to) => from <= placement.start && placement.end <= to

    // A passage yet to be translated, every line of which is replaced by itself, is translated
    // by the same lines.
    val same: List[(Int, Int)] = anchored.bind: (passage, index) =>
      val whole = pairs.count(_(0) == index) == solid(passage.replaced).size
      val kept = !covering(index).exists: other => other.lines != other.replaced
      if passage.pending && whole && kept then List((passage.start, passage.end)) else Nil

    Request(within, lines, same)
