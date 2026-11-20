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
    anchor(hunk, start, Content(reference))

  def anchor(hunk: Hunk, start: Int, reference: Content): Hunk =
    val bare = hunk.copy(before = Nil, after = Nil, start = false, end = false)
    val size = bare.replaced.size

    // An insertion is anchored by a line of context at least, or by the document's start.
    val count: Optional[Int] =
      if reference.slice(start, start + size) != bare.replaced then Unset
      else reference.anchor(start, size, bare.insertion)

    count.lay(bare.copy(before = reference.slice(0, start), start = true)): count =>
      bare.copy(before = reference.slice(start - count, start))

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
    val reference = rendition.base
    val target = Content(lines)
    val size = rendition.lines.size

    // The placements are in order, in the reference and in the rendered content alike, and no
    // two of them overlap in either: each is sought by a binary search.
    val placed: scala.Array[Placement] = placements.stdlib.toArray
    val changes: scala.Array[Region] = regions.stdlib.toArray

    // How many of the placements, which are in order by `key`, have a `key` less than `bound`.
    def fewer(key: Placement => Int, bound: Int): Int =
      var low = 0
      var high = placed.length

      while low < high do
        val middle = (low + high)/2
        if key(placed(middle)) < bound then low = middle + 1 else high = middle

      low

    def widen(region: Region): Interval =
      def holder(line: Int): Optional[Placement] =
        val index = fewer(_.to, line + 1)
        if index < placed.length && placed(index).from <= line then placed(index) else Unset

      def straddler(boundary: Int): Optional[Placement] =
        val index = fewer(_.to, boundary + 1)
        if index < placed.length && placed(index).from < boundary then placed(index) else Unset

      if region.from == region.to
      then
        val within = straddler(region.from)
        Interval(within.let(_.from).or(region.from), within.let(_.to).or(region.to))
      else
        Interval
          ( holder(region.from).let(_.from).or(region.from),
            holder(region.to - 1).let(_.to).or(region.to) )

    // Whether lines were inserted exactly at a boundary of the rendered content.
    val insertions: scala.collection.immutable.Set[Int] =
      scala.collection.immutable.Set.from:
        changes.iterator.filter { region => region.changed && region.from == region.to }.map(_.from)

    def inserted(boundary: Int): Boolean = insertions.contains(boundary)

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
      var low = 0
      var high = changes.length

      while low < high do
        val middle = (low + high)/2
        if changes(middle).to < boundary then low = middle + 1 else high = middle

      val found = scala.collection.mutable.ListBuffer[Int]()

      while low < changes.length && changes(low).from <= boundary do
        val region = changes(low)

        if !region.changed then found += region.from2 + boundary - region.from else
          if region.from == boundary then found += region.from2
          if region.to == boundary then found += region.to2

        low += 1

      List.from(found.toList)

    def before(boundary: Int): Int = positions(boundary).fold(lines.size)(_.min(_))
    def after(boundary: Int): Int = positions(boundary).fold(0)(_.max(_))

    // Where a boundary of the rendered content falls in the reference: after any hunk which ends
    // there, or before any which begins there.
    def beyond(boundary: Int): Int =
      val count = fewer(_.to, boundary + 1)
      if count == 0 then boundary else placed(count - 1).end + boundary - placed(count - 1).to

    def short(boundary: Int): Int =
      val index = fewer(_.from, boundary)

      if index == placed.length then reference.size - (size - boundary)
      else placed(index).start - (placed(index).from - boundary)

    def absorbed(interval: Interval): List[Placement] =
      val found = scala.collection.mutable.ListBuffer[Placement]()
      var index = fewer(_.from, interval.from)

      while index < placed.length && placed(index).from < interval.to do
        val placement = placed(index)

        val within =
          if placement.from == placement.to
          then interval.from < placement.from && placement.from < interval.to
          else interval.from <= placement.from && placement.to <= interval.to

        if within then found += placement
        index += 1

      List.from(found.toList)

    def span(interval: Interval): (Int, Int) =
      val end = short(interval.to)
      (beyond(interval.from).min(end), end)

    // A new hunk must not overlap a hunk it does not replace, or one of the two would be lost:
    // an interval grows to take in any hunk which its span in the reference would overlap.
    def settle(intervals: List[Interval]): List[Interval] =
      val grown = intervals.map: interval =>
        val (start, end) = span(interval)
        val probe = Placement(start, end, Hunk(Nil, Nil), point, true, Nil)
        val within = absorbed(interval).stdlib.toSet
        val found = scala.collection.mutable.ListBuffer[Placement]()
        var index = fewer(_.end, start)

        while index < placed.length && placed(index).start <= end do
          if !within.contains(placed(index)) && placed(index).overlaps(probe) then found += placed(index)
          index += 1

        val rivals = List.from(found.toList)

        rivals.fold(interval): (interval, rival) =>
          Interval(interval.from.min(rival.from), interval.to.max(rival.to))

      if grown == intervals then intervals else settle(merge(grown.order(_.from), Nil))

    val intervals = settle(touched)

    val fresh: List[(Int, Hunk)] = intervals.bind: interval =>
      val (start, end) = span(interval)
      val text = target.slice(before(interval.from), after(interval.to))
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

      (start, Hunk(reference.slice(start, end), text, former = former)) :: stops

    val taken = intervals.bind(absorbed).stdlib.toSet

    val kept: List[(Int, Hunk)] =
      placements.filter: placement => placement.origin == point && !taken.contains(placement)
      . map: placement => (placement.start, placement.hunk)

    (kept + fresh.map { (start, hunk) => (start, anchor(hunk, start, rendition.base)) })
    . order: (start, hunk) => hunk.rank(start)
    . map(_(1))
