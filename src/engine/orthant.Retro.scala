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

import proscenium.{List, Nil}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.sortingAlgorithms.timsort

// Check-ins which store an edit somewhere other than the point it was made at: at an ancestor,
// from which it reaches more points, or at whichever ancestor first had the lines it changes.
// The edit is taken as the changes between the point's content and the new lines; each change
// is carried to the content of the point it is sent to, which must have the same lines there,
// and is checked in at that point. Nothing is merged: a change which cannot be carried, or a
// result which is not the new lines at the point the edit was made at, is refused.
object Retro:
  // The changes by which `old` becomes `lines`, each a hunk where it begins in `old`.
  def changes(old: List[Text], lines: List[Text]): List[(Int, Hunk)] =
    Region.between(old, lines).filter(_.changed).bind: region =>
      val size = region.to - region.from

      // A run of lines which gives way to as many is a change to each line which differs, so
      // that each may be sent to its own place.
      val whole = List((region.from, region.to, region.from2, region.to2))

      val runs: List[(Int, Int, Int, Int)] =
        if size != region.to2 - region.from2 then whole
        else
          List.range(0, size).map: index => (region.from + index, region.from2 + index)
          . filter: (line, line2) => old.excerpt(line, line + 1) != lines.excerpt(line2, line2 + 1)
          . map: (line, line2) => (line, line + 1, line2, line2 + 1)

      runs.map: (from, to, from2, to2) =>
        val hunk = Hunk(old.excerpt(from, to), lines.excerpt(from2, to2))
        (from, Inscription.anchor(hunk, from, old))

  // The content `target` with the changes made to `old` carried onto it, or `Unset` if one of
  // them does not find the lines it was made to.
  def carry(changes: List[(Int, Hunk)], old: List[Text], target: List[Text])
  :   Optional[List[Text]] =

    val outcomes = Rebase(changes, old, target)

    val placed: List[(Int, Hunk)] = outcomes.sweep:
      case Rebase.Outcome.Fresh(start, hunk) => (start, hunk)

    if placed.size != outcomes.size then Unset else
      // From the end backwards, so that no change moves the lines of one yet to be made.
      placed.order { (start, hunk) => -hunk.rank(start) }.fold(target): (content, pair) =>
        val (start, hunk) = pair

        content.excerpt(0, start) + hunk.lines +
          content.excerpt(start + hunk.replaced.size, content.size)

  // The dossier after the changes are checked in, each group at its point, the first group
  // first; `lines` is what the content at `point` must then be.
  private def send
    ( dossier: Dossier, point: Point, old: List[Text], lines: List[Text],
      groups: List[(Point, List[(Int, Hunk)])] )
  :   Dossier raises Axis.Error raises Checkin.Error =

    val result = groups.fold(dossier): (current, group) =>
      val (target, changes) = group
      val content = current.content(target).or(abort(Checkin.Error(target, Checkin.Reason.Carry)))

      // The changes were made to the content of `point` as it was, which the check-ins before
      // this one have not changed where these changes are.
      val carried =
        carry(changes, old, content).or(abort(Checkin.Error(target, Checkin.Reason.Carry)))

      current.checkin(target, carried)

    if result.content(point) != lines then abort(Checkin.Error(point, Checkin.Reason.Unreached))
    result

  // A check-in of `lines`, edited at `point`, stored at `ancestor`, which `point` is below.
  def since(dossier: Dossier, point: Point, lines: Optional[List[Text]], ancestor: Point)
  :   Dossier raises Axis.Error raises Checkin.Error =

    if !dossier.space.below(point, ancestor)
    then abort(Checkin.Error(ancestor, Checkin.Reason.Scope))

    dossier.content(point).lay(dossier.checkin(ancestor, lines)): old =>
      lines.lay(dossier.checkin(ancestor, Unset)): lines =>
        send(dossier, point, old, lines, List((ancestor, changes(old, lines))))

  // A check-in of `lines`, edited at `point`, in which each change to existing lines is stored
  // at the furthest ancestor of `point` along `axis` which has those lines, as every value
  // between does; lines which are only inserted stay at `point`.
  def origin(dossier: Dossier, point: Point, lines: Optional[List[Text]], axis: Axis)
  :   Dossier raises Axis.Error raises Checkin.Error =

    dossier.content(point).lay(dossier.checkin(point, lines)): old =>
      lines.lay(dossier.checkin(point, Unset)): lines =>
        val lineage = axis.lineage(point(axis)).map(point.move(axis, _))

        def source(change: (Int, Hunk)): Point =
          if change(1).insertion then point else
            def climb(todo: List[Point], reached: Point): Point = todo.head.lay(reached): next =>
              val carried = dossier.content(next).let(carry(List(change), old, _))
              if carried.absent then reached else climb(todo.excerpt(1, todo.size), next)

            climb(lineage.excerpt(1, lineage.size), point)

        val sent = changes(old, lines).map: change => (source(change), change)

        // The furthest ancestor first, so that each check-in is seen by those below it.
        val groups = lineage.reverse.bind: target =>
          val group = sent.filter(_(0) == target).map(_(1))
          if group.nil then Nil else List((target, group))

        send(dossier, point, old, lines, groups)
