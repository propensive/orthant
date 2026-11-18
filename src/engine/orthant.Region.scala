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

import proscenium.List
import soundness.*
import soundness.dysasymptotics.linearSize

object Region:
  // The regions by which `left` becomes `right`, in order, covering both wholly.
  def between(left: List[Text], right: List[Text]): List[Region] =
    val tracts = diff(left.to[Sequence], right.to[Sequence]).collate

    tracts.fold(List[Region]()): (regions, tract) =>
      val from = regions.head.let(_.to).or(0)
      val from2 = regions.head.let(_.to2).or(0)

      tract match
        case Tract.Unchanged(retentions) =>
          Region(false, from, from + retentions.size, from2, from2 + retentions.size) :: regions

        case Tract.Changed(deletions, insertions) =>
          Region(true, from, from + deletions.size, from2, from2 + insertions.size) :: regions

    . reverse

// A run of lines of one content and the run of another it corresponds to: either the same lines
// in both, or a change, where the lines `from` to `to` of the first give way to the lines `from2`
// to `to2` of the second. Either side of a change may be empty.
case class Region(changed: Boolean, from: Int, to: Int, from2: Int, to2: Int):
  def pure: Boolean = changed && from2 == to2
