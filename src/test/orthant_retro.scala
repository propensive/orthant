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

// Prints, for each seed, a digest of what check-ins stored elsewhere (at the origin of each
// change along an axis, and at an ancestor) leave, after every step of a run of random
// check-ins, so that two builds may be compared: `java -cp <test jar> orthant.retros 1 2000`.
@main
def retros(from: Int, to: Int): Unit =
  import strategies.throwUnsafely
  import Trials.*

  def digest(dossier: Dossier): Int =
    points.map: point =>
      dossier.folios.at(point).let:
        case delta: Folio.Delta   => Delta.serialize(delta).join(t"\n").s
        case absent: Folio.Absent => Delta.serialize(absent).join(t"\n").s
        case Folio.Base(lines)    => "base "+lines.join(t"\n").s

      . or("none")
      . hashCode

    . stdlib.hashCode

  def attempt(dossier: Dossier)(action: => Dossier): Int =
    try digest(action)
    catch
      case error: Checkin.Error      => ("error "+error.reason.toString).hashCode
      case error: orthant.Axis.Error => "axis".hashCode

  List.range(from, to).each: seed =>
    def recur(dossier: Dossier, step: Int, sum: Int): Int =
      if step == 30 then sum else
        val (after, _) = check(dossier, seed, step)
        val point = pick(points, random(seed, step*10 + 7, points.size))
        val axis = pick(space.axes, random(seed, step*10 + 8, space.axes.size))

        val lines: List[Text] =
          List.range(0, 1 + random(seed, step*10 + 9, 3)).fold(after.content(point).or(Nil)):
            (lines, edit) => mutate(lines, seed + 7, step*10 + edit*3)

        val origin = attempt(after)(Retro.origin(after, point, lines, axis))
        val ancestor = axis.lineage(point(axis)).last.let(point.move(axis, _)).or(point)
        val since = attempt(after)(Retro.since(after, point, lines, ancestor))
        recur(after, step + 1, (sum*31 + origin)*31 + since)

    java.lang.System.out.nn.println(seed.toString+" "+recur(start, 0, 0).toString)
