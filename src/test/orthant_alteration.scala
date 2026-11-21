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

// Prints, for each seed, a digest of the views against a change, and of what reading them back
// (as shown, without their `!` lines, and without their `?` lines) gives, after every step of a
// run of random check-ins, so that two builds may be compared:
// `java -cp <test jar> orthant.alterations 1 2000`.
@main
def alterations(from: Int, to: Int): Unit =
  import strategies.throwUnsafely
  import Trials.*

  def placed(placement: Placement): String =
    placement.start.toString+"-"+placement.end.toString+placement.fresh.toString+
      placement.lines.join(t"|").s

  def read(dossier: Dossier, point: Point, former: List[Text], view: List[Text]): String =
    try
      val request = Indirect.alterationRead(dossier, point, former, view)
      request.checked.map(placed).stdlib.mkString(";")+"#"+request.lines.join(t"|").s+"#"+
        request.spans.map((from, to) => from.toString+"-"+to.toString).stdlib.mkString(";")
    catch case error: Indirect.Error => "error "+error.line.toString+error.reason.toString

  def digest(dossier: Dossier): Int =
    points.filter(point => safely(dossier.translation(point)).or(false)).map: point =>
      List(t"v1", t"v2", t"v3", t"v2x").map: version =>
        val former: List[Text] = safely:
          space.storageParent(point).let: parent =>
            dossier.content(parent.move(release, version))
        . or(Nil)

        safely(Indirect.alterationView(dossier, point, former)).let: view =>
          val bare = view.filter(line => !line.s.startsWith("!"))
          val prompt = view.filter(line => !line.s.startsWith("?"))
          (view.join(t"\n").s+"@"+read(dossier, point, former, view)+"@"+
            read(dossier, point, former, bare)+"@"+read(dossier, point, former, prompt)).hashCode
        . or(0)

      . stdlib.hashCode

    . stdlib.hashCode

  List.range(from, to).each: seed =>
    def recur(dossier: Dossier, step: Int, sum: Int): Int =
      if step == 40 then sum else
        val (after, _) = check(dossier, seed, step)
        recur(after, step + 1, sum*31 + digest(after))

    java.lang.System.out.nn.println(seed.toString+" "+recur(start, 0, 0).toString)
