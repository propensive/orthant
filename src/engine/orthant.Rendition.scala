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

// A hunk in place: it gives `lines` in place of the lines `start` to `end` of the storage
// parent's content. `origin` is the point it is stored at, and `fresh` is false if the lines it
// was written against have since changed. `from` and `to` are where its lines fall in the
// rendered content.
case class Placement
  ( start:  Int,
    end:    Int,
    hunk:   Hunk,
    origin: Point,
    fresh:  Boolean,
    lines:  List[Text],
    from:   Int = 0,
    to:     Int = 0 ):

  def overlaps(other: Placement): Boolean =
    if start == end && other.start == other.end then start == other.start
    else if start == end then other.start < start && start < other.end
    else if other.start == other.end then start < other.start && other.start < end
    else start < other.end && other.start < end

// Something about a point's content that wants attention.
enum Issue:
  // A hunk shows text written against lines which have since changed.
  case Stale(placement: Placement)

  // Hunks from two points, neither below the other, give different text for the same lines.
  case Conflict(shown: Placement, hidden: Placement)

  // A stored hunk's anchor is not in its storage parent's content, or is there more than once.
  case Orphan(origin: Point, hunk: Hunk)
  case Ambiguous(origin: Point, hunk: Hunk)

// The content of a document at a point, with the storage parent's content it was derived from,
// the hunks which were applied to that, in order, and whatever wants attention.
case class Rendition
  ( lines:      List[Text],
    reference:  List[Text]      = Nil,
    placements: List[Placement] = Nil,
    issues:     List[Issue]     = Nil )
  ( shared: Optional[Content] = Unset ):

  // The lines, and the reference's lines, indexed: `shared` is the reference's index where the
  // storage parent's rendition has it already.
  lazy val content: Content = Content(lines)
  lazy val base: Content = shared.or(Content(reference))
