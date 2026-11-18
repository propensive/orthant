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

  private def count(lines: List[Text], content: List[Text]): Int =
    occurrences(Hunk(lines, Nil), content).size

  private case class Hull(from: Int, to: Int, from2: Int, to2: Int, start: Int, hunk: Hunk)

  // The hunks, each at the position its replaced lines begin in `old`, carried onto `next`.
  def apply(placed: List[(Int, Hunk)], old: List[Text], next: List[Text]): List[Outcome] =
    val regions = Region.between(old, next)
    val same = regions.filter(!_.changed)

    def carry(position: Int, region: Region): Int = region.from2 + position - region.from

    def classify(start: Int, hunk: Hunk): Outcome | Hull =
      val end = start + hunk.replaced.size
      val from = start - hunk.before.size
      val to = end + hunk.after.size
      val whole = same.filter { region => region.from <= from && to <= region.to }.head

      def stale(touched: List[Region]): Outcome | Hull =
        val first = touched.head.or(panic(m"a stale hunk touches a region"))
        val last = touched.last.or(panic(m"a stale hunk touches a region"))

        Hull
          ( if first.changed then first.from else start,
            if last.changed then last.to else end,
            if first.changed then first.from2 else carry(start, first),
            if last.changed then last.to2 else carry(end, last),
            start,
            hunk )

      whole.let { region => Outcome.Fresh(carry(start, region), hunk) }.or:
        if hunk.insertion then
          val before = same.filter { region => region.from < start && start <= region.to }.head
          val after = same.filter { region => region.from <= start && start < region.to }.head

          before.or(after).let { region => Outcome.Fresh(carry(start, region), hunk) }.or:
            if start == 0 then Outcome.Fresh(0, hunk)
            else if start == old.size then Outcome.Fresh(next.size, hunk)
            else stale(regions.filter { region => region.from <= start && start <= region.to })
        else
          val within = same.filter { region => region.from <= start && end <= region.to }.head

          within.let: region =>
            if count(hunk.replaced, old) == 1 && count(hunk.replaced, next) == 1
            then Outcome.Fresh(carry(start, region), hunk)
            else Hull(start, end, carry(start, region), carry(end, region), start, hunk)

          . or:
              val touched = regions.filter: region =>
                (region.from < end && start < region.to) ||
                  (region.from == region.to && start < region.from && region.from < end)

              if !touched.exists(!_.pure) then Outcome.Orphan(hunk) else stale(touched)

    // The stale hunks whose hulls overlap are one: its own text is that of `old` across the
    // whole hull, with each of them applied.
    def merge(hulls: List[Hull]): List[Outcome] =
      def own(from: Int, todo: List[Hull], to: Int): List[Text] = todo match
        case hull :: tail =>
          old.excerpt(from, hull.start) + hull.hunk.lines +
            own(hull.start + hull.hunk.replaced.size, tail, to)

        case _ =>
          old.excerpt(from, to)

      def group(todo: List[Hull], current: List[Hull], done: List[List[Hull]]): List[List[Hull]] =
        todo match
          case hull :: tail =>
            val reach = current.map(_.to).fold(-1)(_.max(_))
            val from = current.map(_.from).fold(hull.from)(_.min(_))

            if current.nil || hull.from < reach || (hull.from == from && hull.to == reach)
            then group(tail, hull :: current, done)
            else group(tail, List(hull), current.reverse :: done)

          case _ =>
            (if current.nil then done else current.reverse :: done).reverse

      group(hulls.order(_.from), Nil, Nil).map: hulls =>
        val from = hulls.map(_.from).fold(old.size)(_.min(_))
        val to = hulls.map(_.to).fold(0)(_.max(_))
        val from2 = hulls.map(_.from2).fold(next.size)(_.min(_))
        val to2 = hulls.map(_.to2).fold(0)(_.max(_))

        val former: List[Text] = hulls match
          case hull :: Nil if hull.hunk.stale => hull.hunk.former
          case _                              => old.excerpt(from, to)

        // An insertion comes before a replacement which begins where it is.
        val lines = own(from, hulls.order { hull => hull.hunk.rank(hull.start) }, to)
        val replaced = next.excerpt(from2, to2)

        // A hunk which finds again the very lines it was written against is stale no longer.
        if former.nil || former == replaced then Outcome.Fresh(from2, Hunk(replaced, lines))
        else Outcome.Stale(from2, to2, Hunk(replaced, lines, former = former))

    val outcomes = placed.map: (start, hunk) => classify(start, hunk)
    val hulls = outcomes.sweep { case hull: Hull => hull }

    // Each outcome with the rank its hunk had in `old`; for hunks which became one, the least.
    val alone: List[(Int, Outcome)] = placed.zip(outcomes).sweep:
      case ((start, hunk), outcome: Outcome) => (hunk.rank(start), outcome)

    val merged: List[(Int, Outcome)] = merge(hulls).map: outcome =>
      val span: (Int, Int) = outcome match
        case Outcome.Stale(start, end, _) => (start, end)
        case Outcome.Fresh(start, hunk)   => (start, start + hunk.replaced.size)
        case Outcome.Orphan(_)            => (0, 0)

      val within = hulls.filter: hull => span(0) <= hull.from2 && hull.to2 <= span(1)
      val ranks = within.map: hull => hull.hunk.rank(hull.start)
      (ranks.fold(old.size*2 + 1)(_.min(_)), outcome)

    val ranked: List[(Int, Outcome)] = alone + merged

    // Where an outcome replaces no lines: the gap at which it inserts its own.
    def gap(outcome: Outcome): Optional[Int] = outcome match
      case Outcome.Fresh(start, hunk) if hunk.insertion        => start
      case Outcome.Stale(start, end, _) if start == end        => start
      case _                                                   => Unset

    // Hunks which were apart, and come to insert at the same place because the lines between
    // them are gone, are one insertion there, of the lines of each in the order they were in.
    val gaps: List[Int] = ranked.map(_(1)).map(gap(_)).sweep { case gap: Int => gap }

    val joined: List[Outcome] = gaps.distinct.map: place =>
      ranked.filter { (rank, outcome) => gap(outcome) == place }.order(_(0)).map(_(1)) match
        case outcome :: Nil =>
          outcome

        case several =>
          val hunks = several.sweep:
            case Outcome.Fresh(_, hunk)    => hunk
            case Outcome.Stale(_, _, hunk) => hunk

          val hunk = Hunk(Nil, hunks.bind(_.lines), former = hunks.bind(_.former))
          if hunk.stale then Outcome.Stale(place, place, hunk) else Outcome.Fresh(place, hunk)

    ranked.map(_(1)).filter(gap(_).absent) + joined
