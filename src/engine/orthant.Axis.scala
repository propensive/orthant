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

import proscenium.{List, Nil, Map, `::`}
import soundness.*

// A value of an axis: its name, the value it cascades from (`Unset` for the root), and whether
// the edge to that parent is a translation, where every paragraph is replaced and the pairing of
// passages is kept as written, or a variation, where most lines are shared.
case class Value(name: Text, parent: Optional[Text] = Unset, translation: Boolean = false)

// What a stale hunk on an axis shows: its own lines, or its storage parent's.
enum Preference:
  case Own, Parent

object Axis:
  enum Reason:
    case Unknown(value: Text)
    case Roots(count: Int)
    case Cycle(value: Text)

  case class Error(axis: Text, reason: Reason)(using Diagnostics)
  extends fulminate.Error
    ( reason match
        case Reason.Unknown(value) => m"the value $value is not on the axis $axis"
        case Reason.Roots(count)   => m"the axis $axis has $count roots, but must have one"
        case Reason.Cycle(value)   => m"the parents of $value on the axis $axis form a cycle" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Unknown(_) => Orthant.bolt(2, 1)
      case Reason.Roots(_)   => Orthant.bolt(2, 2)
      case Reason.Cycle(_)   => Orthant.bolt(2, 3)

  // An axis whose values are checked to form a tree: one root, every parent present, no cycle.
  def apply(name: Text, values: List[Value], preference: Preference = Preference.Own)
  :   Axis raises Error =

    val index: Map[Text, Value] = Map.from(values.map { value => value.name -> value }.stdlib)
    val roots = values.filter(_.parent.absent)

    val root: Text = roots match
      case root :: Nil => root.name
      case _           => abort(Error(name, Reason.Roots(roots.count { value => true })))

    def climb(value: Value, seen: List[Text]): Unit = value.parent.let: parent =>
      if seen.has(parent) then abort(Error(name, Reason.Cycle(value.name)))
      climb(index.at(parent).or(abort(Error(name, Reason.Unknown(parent)))), parent :: seen)

    values.each: value => climb(value, List(value.name))

    new Axis(name, values, preference, index, root)

// A named tree of values. Its order among the axes of a `Space` is its precedence.
class Axis private
  ( val name: Text, val values: List[Value], val preference: Preference,
    index: Map[Text, Value], val root: Text ):

  def value(name: Text): Value raises Axis.Error =
    index.at(name).or(abort(Axis.Error(this.name, Axis.Reason.Unknown(name))))

  def has(name: Text): Boolean = index.defines(name)

  // The value and its ancestors, nearest first, ending in the root.
  def lineage(name: Text): List[Text] raises Axis.Error =
    name :: value(name).parent.lay(Nil)(lineage(_))

  // How many edges separate the value from the root.
  def depth(name: Text): Int raises Axis.Error = value(name).parent.lay(0)(depth(_) + 1)

  // Whether `lower` is `upper` or a descendant of it.
  def below(lower: Text, upper: Text): Boolean raises Axis.Error = lineage(lower).has(upper)

  def children(name: Text): List[Text] = values.filter(_.parent == name).map(_.name)
