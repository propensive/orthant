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
import soundness.{Holding as _, *}

// One stored change: some consecutive lines of the storage parent's content, `replaced`, give way
// to `lines`. The lines of context `before` and `after` it, with `start` and `end` where the
// context reaches an end of the document, serve only to locate it; together with the replaced
// lines they are its anchor. A hunk with `former` lines is stale: they are what it replaced
// before the storage parent changed beneath it.
case class Hunk
  ( replaced: List[Text],
    lines:    List[Text],
    before:   List[Text] = Nil,
    after:    List[Text] = Nil,
    former:   List[Text] = Nil,
    start:    Boolean    = false,
    end:      Boolean    = false ):

  def stale: Boolean = !former.nil
  def insertion: Boolean = replaced.nil

  // The order of a hunk whose replaced lines begin at `start` among the hunks of a delta: by
  // position, an insertion coming before a replacement which begins where it is.
  def rank(start: Int): Int = start*2 + (if insertion then 0 else 1)

// What the store holds for a document at a point: its full text, the fact of its absence, or the
// hunks by which it differs from the storage parent's content, on the named storage axis.
enum Holding:
  case Base(lines: List[Text])
  case Absent(axis: Text)
  case Delta(axis: Text, hunks: List[Hunk])
