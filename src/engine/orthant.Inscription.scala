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

// Inscription: finding the hunks to store at a point so that its content becomes some new lines.
object Inscription:
  // The hunk with the least context before it that makes its anchor occur once, where it is, in
  // the reference; if every line before it is not enough, it is anchored to the document's start.
  def anchor(hunk: Hunk, start: Int, reference: List[Text]): Hunk =
    val bare = hunk.copy(before = Nil, after = Nil, start = false, end = false)

    def recur(count: Int): Hunk =
      val candidate = bare.copy(before = reference.excerpt(start - count, start))

      // An insertion is anchored by a line of context at least, or by the document's start.
      val anchored = !bare.insertion || count > 0

      if anchored && Rebase.occurrences(candidate, reference) == List(start) then candidate
      else if count >= start then candidate.copy(start = true)
      else recur(count + 1)

    recur(0)

  // A run of the rendered content, between two of its line boundaries, which a change touches,
  // widened so that it cuts through no hunk.
  private case class Interval(from: Int, to: Int)

  // The hunks to store at `point` so that the rendition's content becomes `lines`: those already
  // stored there which the change does not touch, and a new one for each run it does touch, which
  // takes the place of every hunk within that run.
  // An edited stale hunk stays stale if `keep` is set: where a stale hunk shows its own lines.
  def hunks(rendition: Rendition, lines: List[Text], point: Point, keep: Boolean = true)
  :   List[Hunk] =

    val regions = Region.between(rendition.lines, lines)
    val placements = rendition.placements
    val reference = rendition.reference
    val size = rendition.lines.size

    def widen(region: Region): Interval =
      def holder(line: Int): Optional[Placement] =
        placements.filter { placement => placement.from <= line && line < placement.to }.head

      def straddler(boundary: Int): Optional[Placement] =
        placements.filter { placement => placement.from < boundary && boundary < placement.to }.head

      if region.from == region.to
      then
        val within = straddler(region.from)
        Interval(within.let(_.from).or(region.from), within.let(_.to).or(region.to))
      else
        Interval
          ( holder(region.from).let(_.from).or(region.from),
            holder(region.to - 1).let(_.to).or(region.to) )

    // Whether lines were inserted exactly at a boundary of the rendered content.
    def inserted(boundary: Int): Boolean = regions.exists: region =>
      region.changed && region.from == boundary && region.to == boundary

    // Intervals which overlap are one, as are two which meet where one of them is empty, or where
    // lines were inserted, which would otherwise belong to both.
    def merge(todo: List[Interval], done: List[Interval]): List[Interval] = todo match
      case next :: tail => done match
        case last :: rest
        if next.from < last.to ||
          ( next.from == last.to &&
            (next.from == next.to || last.from == last.to || inserted(next.from)) ) =>
          merge(tail, Interval(last.from.min(next.from), last.to.max(next.to)) :: rest)

        case _ =>
          merge(tail, next :: done)

      case _ =>
        done.reverse

    val touched = merge(regions.filter(_.changed).map(widen).order(_.from), Nil)
    // Where a boundary of the rendered content falls in the new lines: before anything inserted
    // there, or after it. A boundary is never within a change, only at an end of one.
    def positions(boundary: Int): List[Int] =
      regions.filter { region => region.from <= boundary && boundary <= region.to }.bind: region =>
        if !region.changed then List(region.from2 + boundary - region.from)
        else
          (if region.from == boundary then List(region.from2) else Nil) +
            (if region.to == boundary then List(region.to2) else Nil)

    def before(boundary: Int): Int = positions(boundary).fold(lines.size)(_.min(_))
    def after(boundary: Int): Int = positions(boundary).fold(0)(_.max(_))

    // Where a boundary of the rendered content falls in the reference: after any hunk which ends
    // there, or before any which begins there.
    def beyond(boundary: Int): Int =
      placements.filter(_.to <= boundary).last.let: last => last.end + boundary - last.to
      . or(boundary)

    def short(boundary: Int): Int =
      placements.filter(_.from >= boundary).head.let: next => next.start - (next.from - boundary)
      . or(reference.size - (size - boundary))

    def absorbed(interval: Interval): List[Placement] = placements.filter: placement =>
      if placement.from == placement.to
      then interval.from < placement.from && placement.from < interval.to
      else interval.from <= placement.from && placement.to <= interval.to

    def span(interval: Interval): (Int, Int) =
      val end = short(interval.to)
      (beyond(interval.from).min(end), end)

    // A new hunk must not overlap a hunk it does not replace, or one of the two would be lost:
    // an interval grows to take in any hunk which its span in the reference would overlap.
    def settle(intervals: List[Interval]): List[Interval] =
      val grown = intervals.map: interval =>
        val (start, end) = span(interval)
        val probe = Placement(start, end, Hunk(Nil, Nil), point, true, Nil)
        val within = absorbed(interval)

        val rivals =
          placements.filter: placement => !within.has(placement) && placement.overlaps(probe)

        rivals.fold(interval): (interval, rival) =>
          Interval(interval.from.min(rival.from), interval.to.max(rival.to))

      if grown == intervals then intervals else settle(merge(grown.order(_.from), Nil))

    val intervals = settle(touched)

    val fresh: List[(Int, Hunk)] = intervals.bind: interval =>
      val (start, end) = span(interval)
      val text = lines.excerpt(before(interval.from), after(interval.to))
      val probe = Placement(start, end, Hunk(Nil, Nil), point, true, Nil)

      // A stale hunk which is edited, alone, is still stale: only confirming it clears that.
      val former: List[Text] = absorbed(interval) match
        case placement :: Nil
        if keep && !placement.fresh && placement.from == interval.from &&
          placement.to == interval.to =>
          placement.hunk.former

        case _ =>
          Nil

      // An insertion which the new hunk replaces, but which lies at an end of its span and so
      // would be inherited all the same, is stopped by a hunk which inserts nothing there.
      val stops = absorbed(interval).filter(!_.overlaps(probe)).map(_.start).distinct.map: gap =>
        (gap, Hunk(Nil, Nil))

      (start, Hunk(reference.excerpt(start, end), text, former = former)) :: stops

    val taken = intervals.bind(absorbed)

    val kept: List[(Int, Hunk)] =
      placements.filter: placement => placement.origin == point && !taken.has(placement)
      . map: placement => (placement.start, placement.hunk)

    (kept + fresh.map { (start, hunk) => (start, anchor(hunk, start, reference)) })
    . order: (start, hunk) => hunk.rank(start)
    . map(_(1))
