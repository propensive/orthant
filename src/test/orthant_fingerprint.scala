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

// A digest of the store which a run of random check-ins leaves and of everything derived from
// it, so that two builds may be compared: `java -cp <test jar> orthant.fingerprint 1 2000`
// prints one for each seed. The suite holds the digest of the first of them, as it was when
// the engine was last changed without changing what is stored or derived.
object Digests:
  import strategies.throwUnsafely
  import Trials.*

  def digest(dossier: Dossier): Int =
    points.map: point =>
      val folio: String = dossier.folios.at(point).let:
        case delta: Folio.Delta   => Delta.serialize(delta).join(t"\n").s
        case absent: Folio.Absent => Delta.serialize(absent).join(t"\n").s
        case Folio.Base(lines)    => "base "+lines.join(t"\n").s

      . or("none")

      val derived: String = safely(dossier.rendition(point)).let: rendition =>
        val placements = rendition.placements.map: placement =>
          placement.start.toString + "-" + placement.end.toString + placement.fresh.toString +
            name(placement.origin).s + placement.lines.join(t"|").s + placement.from.toString

        rendition.lines.join(t"\n").s + "#" + placements.stdlib.mkString(";") + "#" +
          rendition.issues.size.toString + "#" + dossier.pending(point).stdlib.mkString(",")

      . or("absent")

      (folio+"@"+derived).hashCode

    . stdlib.hashCode

  def of(seed: Int): Int =
    def recur(dossier: Dossier, step: Int, sum: Int): Int =
      if step == 40 then sum else
        val (after, failure) = check(dossier, seed, step)
        recur(after, step + 1, sum*31 + digest(after) + failure.let(_.s.hashCode).or(0))

    recur(start, 0, 0)

  // One number for the seeds from 1 to `count`.
  def first(count: Int): Int = List.range(1, count + 1).map(of).stdlib.hashCode

@main
def fingerprint(from: Int, to: Int): Unit =
  List.range(from, to).each: seed =>
    java.lang.System.out.nn.println(seed.toString+" "+Digests.of(seed).toString)

@main
def fingerprints(count: Int): Unit =
  java.lang.System.out.nn.println(Digests.first(count).toString)
