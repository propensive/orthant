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

// What wants doing: for each document at each point, the lines which are yet to be translated,
// the passages which are stale, and whatever else is wrong; and where best to do it.
object Survey:
  // The work in one document at one point. `total` is how many lines there are to translate,
  // done or not; `pending` is those which are not, and `faults` counts the hunks which
  // conflict, or which have no place.
  case class Work
    ( document: Text, total: Int, pending: List[Text], stale: List[Placement], faults: Int ):
    def wanting: Boolean = !pending.nil || !stale.nil || faults > 0

  // The work at one point, over several documents.
  case class Sum(total: Int, pending: Int, stale: Int, faults: Int):
    def wanting: Boolean = pending > 0 || stale > 0 || faults > 0
    def done: Int = total - pending
    def percent: Int = if total == 0 then 100 else done*100/total

  def work(store: Store, document: Text, point: Point): Work raises Axis.Error =
    val dossier = store.dossier(document)

    store.recall(document, point):
      dossier.rendition(point).lay(Work(document, 0, Nil, Nil, 0)): rendition =>
        val total = if dossier.translation(point) then rendition.reference.count(_ != t"") else 0
        val pending = dossier.pending(point).map(rendition.base(_))
        val stale = rendition.issues.sweep { case Issue.Stale(placement) => placement }

        Work(document, total, pending, stale, rendition.issues.size - stale.size)

  def sum(works: List[Work]): Sum =
    Sum
      ( works.map(_.total).fold(0)(_ + _), works.map(_.pending.size).fold(0)(_ + _),
        works.map(_.stale.size).fold(0)(_ + _), works.map(_.faults).fold(0)(_ + _) )

  def sum(store: Store, point: Point): Sum raises Axis.Error =
    sum(store.documents.map(work(store, _, point)))

  // The points whose hunks a point inherits, itself apart: work done at one of them is done
  // for the point as well.
  def above(store: Store, point: Point): List[Point] raises Axis.Error =
    store.space.origins(point).filter(_ != point)

  private def key(placement: Placement): (Point, List[Text]) =
    (placement.origin, placement.hunk.lines)

  // The work in a document at a point which is not also work at a point above it: that which
  // is best done there.
  def own(store: Store, document: Text, point: Point): Work raises Axis.Error =
    val here = work(store, document, point)
    val others = above(store, point).map(work(store, document, _))
    val pending = others.bind(_.pending).stdlib.toSet
    val stale = others.bind(_.stale.map(key)).stdlib.toSet

    here.copy
      ( pending = here.pending.filter(!pending.contains(_)),
        stale   = here.stale.filter { placement => !stale.contains(key(placement)) } )
