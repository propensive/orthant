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

import proscenium.{List, Nil, `::`}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.sortingAlgorithms.timsort

// Re-basing: carrying hunks located in one content onto another which that content has become,
// or which corresponds to it at another point. It is the one primitive by which a hunk is
// inherited along an axis and by which stored hunks follow a change to their storage parent.
object Rebase:
  enum Outcome:
    // The hunk applies as written, its replaced lines beginning at `start` of the new content.
    case Fresh(start: Int, hunk: Hunk)

    // The lines the hunk replaced have changed: `hunk` is its stale form, replacing the lines
    // `start` to `end` of the new content, with what it formerly replaced as its former lines.
    case Stale(start: Int, end: Int, hunk: Hunk)

    // The lines the hunk replaced were deleted, so it applies to nothing.
    case Orphan(hunk: Hunk)

  // Where the anchor of a hunk occurs in a content: each position at which its replaced lines
  // would begin.
  def occurrences(hunk: Hunk, content: List[Text]): List[Int] =
    val block = hunk.before + hunk.replaced + hunk.after
    val size = content.size

    List.range(0, size - block.size + 1).filter: index =>
      (!hunk.start || index == 0) && (!hunk.end || index + block.size == size) &&
        content.excerpt(index, index + block.size) == block

    . map(_ + hunk.before.size)

  // Where the replaced lines of a hunk begin in a content, if its anchor occurs there once;
  // `Content.Absent` if it does not occur, and `Content.Several` if it occurs more than once.
  def locate(hunk: Hunk, content: Content): Int =
    val found = content.locate(hunk.before + hunk.replaced + hunk.after, hunk.start, hunk.end)
    if found < 0 then found else found + hunk.before.size

  private case class Hull(from: Int, to: Int, from2: Int, to2: Int, start: Int, hunk: Hunk)

  def apply(placed: List[(Int, Hunk)], old: List[Text], next: List[Text]): List[Outcome] =
    apply(placed, Content(old), Content(next))

  // The regions by which `old` becomes `next`, found once, and how a hunk of `old` fares on them.
  private final class Carrier(old: Content, next: Content):
    val regions: scala.Array[Region] = Region.between(old.lines, next.lines).stdlib.toArray
    val same: scala.Array[Region] = regions.filter(!_.changed)

    // How many of the regions before each are not pure deletions.
    val impure: scala.Array[Int] = regions.scanLeft(0): (sum, region) =>
      sum + (if region.pure then 0 else 1)

    // The index of the first of `among` for which `test` holds, where it holds for every one
    // after the first for which it does; the size of `among` if it holds for none.
    def first(among: scala.Array[Region])(test: Region => Boolean): Int =
      Content.search(among.length): index => test(among(index))

    // The first unchanged region which reaches `to` or beyond, if it satisfies `test`.
    def reaching(to: Int, strict: Boolean)(test: Region => Boolean): Optional[Region] =
      val index = first(same): region => if strict then region.to > to else region.to >= to
      if index < same.length && test(same(index)) then same(index) else Unset

    def carry(position: Int, region: Region): Int = region.from2 + position - region.from

    def classify(start: Int, hunk: Hunk): Outcome | Hull =
      val end = start + hunk.replaced.size
      val from = start - hunk.before.size
      val to = end + hunk.after.size
      val whole = reaching(to, false)(_.from <= from)

      // The regions from index `low` up to, not including, index `high`.
      def stale(low: Int, high: Int): Outcome | Hull =
        if low >= high then panic(m"a stale hunk touches a region")
        val first = regions(low)
        val last = regions(high - 1)

        Hull
          ( if first.changed then first.from else start,
            if last.changed then last.to else end,
            if first.changed then first.from2 else carry(start, first),
            if last.changed then last.to2 else carry(end, last),
            start,
            hunk )

      whole.let { region => Outcome.Fresh(carry(start, region), hunk) }.or:
        if hunk.insertion then
          val before = reaching(start, false)(_.from < start)
          val after = reaching(start, true)(_.from <= start)

          before.or(after).let { region => Outcome.Fresh(carry(start, region), hunk) }.or:
            if start == 0 then Outcome.Fresh(0, hunk)
            else if start == old.size then Outcome.Fresh(next.size, hunk)
            else
              // Every region which `start` is within or at an end of.
              stale(first(regions)(_.to >= start), first(regions)(_.from > start))
        else
          val within = reaching(end, false)(_.from <= start)

          within.let: region =>
            if old.locate(hunk.replaced) >= 0 && next.locate(hunk.replaced) >= 0
            then Outcome.Fresh(carry(start, region), hunk)
            else Hull(start, end, carry(start, region), carry(end, region), start, hunk)

          . or:
            // Every region which overlaps the replaced lines, and every empty one within them.
            val low = first(regions): region =>
              region.to > start && (region.from < region.to || region.from > start)

            val high = first(regions)(_.from >= end)

            if high <= low || impure(high) - impure(low) == 0 then Outcome.Orphan(hunk)
            else stale(low, high)

    def merge(hulls: List[Hull]): List[Outcome] =
      def own(from: Int, todo: List[Hull], to: Int): List[Text] =
        val buffer = scala.collection.mutable.ListBuffer[Text]()

        val position = todo.fold(from): (position, hull) =>
          buffer ++= old.slice(position, hull.start).stdlib
          buffer ++= hull.hunk.lines.stdlib
          hull.start + hull.hunk.replaced.size

        buffer ++= old.slice(position, to).stdlib
        List.from(buffer.toList)

      def group(sorted: List[Hull]): List[List[Hull]] =
        val done = scala.collection.mutable.ListBuffer[List[Hull]]()

        // The hulls of `todo` grouped onto `done`, the group so far being `current`, which spans
        // from `least` to `reach`.
        @tailrec
        def recur(todo: scala.List[Hull], current: scala.List[Hull], least: Int, reach: Int)
        :   Unit =

          if todo.isEmpty then
            if !current.isEmpty then done += List.from(current.reverse)
          else
            val hull = todo.head

            if current.isEmpty || hull.from < reach || (hull.from == least && hull.to == reach)
            then
              val least2 = if current.isEmpty then hull.from else least.min(hull.from)
              recur(todo.tail, hull :: current, least2, reach.max(hull.to))
            else
              done += List.from(current.reverse)
              recur(todo.tail, scala.List(hull), hull.from, hull.to)

        recur(sorted.stdlib, scala.Nil, 0, -1)
        List.from(done.toList)

      group(hulls.order(_.from)).map: hulls =>
        val from = hulls.map(_.from).fold(old.size)(_.min(_))
        val to = hulls.map(_.to).fold(0)(_.max(_))
        val from2 = hulls.map(_.from2).fold(next.size)(_.min(_))
        val to2 = hulls.map(_.to2).fold(0)(_.max(_))

        val former: List[Text] = hulls match
          case hull :: Nil if hull.hunk.stale => hull.hunk.former
          case _                              => old.slice(from, to)

        val lines = own(from, hulls.order { hull => hull.hunk.rank(hull.start) }, to)
        val replaced = next.slice(from2, to2)

        if former.nil || former == replaced then Outcome.Fresh(from2, Hunk(replaced, lines))
        else Outcome.Stale(from2, to2, Hunk(replaced, lines, former = former))

    // The outcome of each hunk as if it were carried alone.
    def alone(start: Int, hunk: Hunk): Outcome = classify(start, hunk) match
      case outcome: Outcome => outcome
      case hull: Hull       => merge(List(hull)).head.or(Outcome.Orphan(hunk))

  // Each hunk carried onto `next` by itself, as `apply` would carry it were it the only one,
  // with the regions between the two found only once.
  def alone(placed: List[(Int, Hunk)], old: Content, next: Content): List[Outcome] =
    val carrier = Carrier(old, next)
    placed.map(carrier.alone(_, _))

  // The hunks, each at the position its replaced lines begin in `old`, carried onto `next`.
  def apply(placed: List[(Int, Hunk)], old: Content, next: Content): List[Outcome] =
    val carrier = Carrier(old, next)
    import carrier.{classify, merge}

    val outcomes = placed.map: (start, hunk) => classify(start, hunk)
    val hulls = outcomes.sweep { case hull: Hull => hull }
    val byFrom2: scala.Array[Hull] = hulls.stdlib.toArray.sortBy(_.from2)

    // The index of the first hull, by where it begins in `next`, which begins at `from2` or after.
    def lowest(from2: Int): Int = Content.search(byFrom2.length)(byFrom2(_).from2 >= from2)

    // Each outcome with the rank its hunk had in `old`; for hunks which became one, the least.
    val alone: List[(Int, Outcome)] = placed.zip(outcomes).sweep:
      case ((start, hunk), outcome: Outcome) => (hunk.rank(start), outcome)

    val merged: List[(Int, Outcome)] = merge(hulls).map: outcome =>
      val span: (Int, Int) = outcome match
        case Outcome.Stale(start, end, _) => (start, end)
        case Outcome.Fresh(start, hunk)   => (start, start + hunk.replaced.size)
        case Outcome.Orphan(_)            => (0, 0)

      // The least rank of the hulls from `index` on which lie within the span.
      @tailrec
      def least(index: Int, rank: Int): Int =
        if index >= byFrom2.length || byFrom2(index).from2 > span(1) then rank else
          val hull = byFrom2(index)
          val within = hull.to2 <= span(1)
          least(index + 1, if within then rank.min(hull.hunk.rank(hull.start)) else rank)

      (least(lowest(span(0)), old.size*2 + 1), outcome)

    val ranked: List[(Int, Outcome)] = alone + merged

    // Where an outcome replaces no lines: the gap at which it inserts its own.
    def gap(outcome: Outcome): Optional[Int] = outcome match
      case Outcome.Fresh(start, hunk) if hunk.insertion        => start
      case Outcome.Stale(start, end, _) if start == end        => start
      case _                                                   => Unset

    // Hunks which were apart, and come to insert at the same place because the lines between
    // them are gone, are one insertion there, of the lines of each in the order they were in.
    // The outcomes at each gap, the gaps in the order they are first met.
    type Found = scala.collection.mutable.ListBuffer[(Int, Outcome)]
    val gaps = scala.collection.mutable.LinkedHashMap[Int, Found]()

    ranked.each: pair =>
      gap(pair(1)).let: place =>
        gaps.getOrElseUpdate(place, scala.collection.mutable.ListBuffer()) += pair

    val joined: List[Outcome] = List.from(gaps.toList).map: (place, found) =>
      List.from(found.toList).order(_(0)).map(_(1)) match
        case outcome :: Nil =>
          outcome

        case several =>
          val hunks = several.sweep:
            case Outcome.Fresh(_, hunk)    => hunk
            case Outcome.Stale(_, _, hunk) => hunk

          val hunk = Hunk(Nil, hunks.bind(_.lines), former = hunks.bind(_.former))
          if hunk.stale then Outcome.Stale(place, place, hunk) else Outcome.Fresh(place, hunk)

    ranked.map(_(1)).filter(gap(_).absent) + joined
