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

import proscenium.{List, Map, `::`}
import soundness.*
import soundness.sortingAlgorithms.timsort

// One value on each axis, keyed by the axis's name. A point of a `Space` is total: it names a
// value for every axis.
case class Point(coordinates: Map[Text, Text]):
  def apply(axis: Axis): Text = coordinates.at(axis.name).or(axis.root)
  def move(axis: Axis, value: Text): Point = Point(coordinates.define(axis.name, value))

// The axes, in order of precedence, and the lattice of points they span.
case class Space(axes: List[Axis]):
  val root: Point = Point(Map.from(axes.map { axis => axis.name -> axis.root }.stdlib))

  // The axis whose values are commits of a source, if there is one.
  val sourced: Optional[Axis] = axes.filter(_.sourced).head

  // Whether the content at a point is the source's alone: it is at the root of every axis but
  // the sourced one. Nothing is stored at such a point.
  def source(point: Point): Boolean = sourced.lay(false): sourced =>
    !axes.exists: axis => axis.name != sourced.name && point(axis) != axis.root

  // The point at the given values, and at the root of every axis not named.
  def point(values: (Text, Text)*): Point raises Axis.Error =
    values.foldLeft(root): (point, pair) =>
      val axis = axes.filter(_.name == pair(0)).head.or:
        abort(Axis.Error(pair(0), Axis.Reason.Unknown(pair(1))))

      axis.value(pair(1))
      point.move(axis, pair(1))

  // The first axis, in order, on which the point is not at the root.
  def storageAxis(point: Point): Optional[Axis] =
    axes.filter { axis => point(axis) != axis.root }.head

  def storageParent(point: Point): Optional[Point] raises Axis.Error =
    storageAxis(point).let: axis => point.move(axis, axis.value(point(axis)).parent.or(axis.root))

  // Whether each value of `lower` is the corresponding value of `upper` or a descendant of it.
  def below(lower: Point, upper: Point): Boolean raises Axis.Error =
    !axes.exists: axis => !axis.below(lower(axis), upper(axis))

  // The depth of the point on each axis, in axis order: the more specific of two comparable
  // points has the greater depths, and the earliest axis decides between incomparable ones.
  def depths(point: Point): List[Int] raises Axis.Error =
    axes.map: axis => axis.depth(point(axis))

  // The points whose hunks a point inherits: those with its value on its storage axis, the root
  // on every earlier axis, and its value or an ancestor of it on every later axis; itself first
  // among them. They are in no particular order.
  def origins(point: Point): List[Point] raises Axis.Error =
    storageAxis(point).lay(List(point)): axis =>
      def recur(todo: List[Axis], points: List[Point]): List[Point] = todo match
        case head :: tail =>
          if head.name == axis.name || point(head) == head.root then recur(tail, points)
          else
            val lineage = head.lineage(point(head))
            recur(tail, points.bind { origin => lineage.map(origin.move(head, _)) })

        case _ =>
          points

      recur(axes, List(point))

  // The points are asked for often, and are found once.
  private val every: java.util.concurrent.atomic.AtomicReference[List[Point]] =
    java.util.concurrent.atomic.AtomicReference()

  // Every point, each after all the points it is below.
  def points: List[Point] raises Axis.Error =
    Optional(every.get).or:
      val found = arrange
      every.set(found)
      found

  private def arrange: List[Point] raises Axis.Error =
    def recur(todo: List[Axis], points: List[Point]): List[Point] = todo match
      case head :: tail =>
        val values = head.values.map(_.name)
        recur(tail, points.bind { point => values.map(point.move(head, _)) })

      case _ =>
        points

    // The sort keeps points of one depth in the order they were made in.
    recur(axes, List(root)).map { point => (depths(point).fold(0)(_ + _), point) }.order(_(0))
    . map(_(1))
