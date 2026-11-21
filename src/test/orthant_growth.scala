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

// A translated document of a given size, three releases long, with a hunk for every line of the
// translation, and the time each operation on it takes, so that the growth of each with the size
// can be seen: `java -cp <test jar> orthant.growthTimes 1000 2000 4000`.
object Growth:
  import strategies.throwUnsafely

  val language = orthant.Axis(t"language", List(Value(t"en"), Value(t"fr", t"en", translation = true)))
  val release = orthant.Axis(t"release", List(Value(t"v1"), Value(t"v2", t"v1"), Value(t"v3", t"v2")))
  val space = Space(List(language, release))

  def at(language: Text, release: Text): Point =
    space.point(t"language" -> language, t"release" -> release)

  def line(index: Int): Text = if index%7 == 6 then t"" else t"Line $index of the play."

  def french(lines: List[Text]): List[Text] =
    lines.map { text => if text == t"" then text else t"Fr: $text" }

  // The document's English at each release, and the dossier holding it and the translation.
  // Each release changes ten lines and inserts five, wherever the document's size puts them.
  def make(size: Int): (List[Text], Dossier) =
    val first = List.range(0, size).map(line)
    val revise = (size/10).max(1)
    val insert = (size/5).max(1)

    val second = first.zip(List.range(0, size)).map: (text, index) =>
      if index%revise == revise/2 && text != t"" then t"$text Revised." else text

    val third = second.zip(List.range(0, size)).bind: (text, index) =>
      if index%insert == insert/2 then List(text, t"A new line after $index.") else List(text)

    val dossier =
      Dossier(space)
      . checkin(at(t"en", t"v1"), first)
      . checkin(at(t"fr", t"v1"), french(first))
      . checkin(at(t"en", t"v2"), second)
      . checkin(at(t"en", t"v3"), third)

    (second, dossier)

  // The lines with ten of them, spread through the document, changed. The difference algorithm
  // the spec prescribes (§10) takes time in proportion to the size times the number of lines
  // changed, so the change is kept the same size as the document grows.
  def edited(lines: List[Text]): List[Text] =
    val step = (lines.size/10).max(1)

    lines.zip(List.range(0, lines.size)).map: (text, index) =>
      if index%step == step/2 then t"$text Edited." else text

  // The nanoseconds each operation takes, in order: derivation of every point, the pending
  // lines, a check-in of ten lines of the translation edited, the indirect view, the view
  // against a change.
  def operations(second: List[Text], dossier: Dossier): List[Long] =
    def timed(block: => Unit): Long =
      val start = java.lang.System.nanoTime
      block
      java.lang.System.nanoTime - start

    val fresh = Dossier(space, dossier.folios)
    val target = at(t"fr", t"v3")

    List
      ( timed(space.points.each(fresh.content(_))),
        timed(space.points.each(fresh.pending(_))),
        timed(fresh.checkin(target, edited(fresh.content(target).or(Nil)))),
        timed(Indirect.view(fresh, target)),
        timed(Indirect.alterationView(fresh, target, second)) )

  // The best of three runs, after one to warm up, of all the operations together.
  def cost(size: Int): Long =
    val (second, dossier) = make(size)
    def once(): Long = operations(second, dossier).fold(0L)(_ + _)
    once()
    List(once(), once(), once()).fold(Long.MaxValue)(_.min(_))

@main
def growthTimes(sizes: Int*): Unit =
  sizes.foreach: size =>
    val (second, dossier) = Growth.make(size)
    Growth.operations(second, dossier)

    val best = List.range(0, 3).map(_ => Growth.operations(second, dossier)).fold(List[Long]()):
      (best, next) => if best.nil then next else best.zip(next).map(_.min(_))

    java.lang.System.out.nn.println(size.toString+": "+best.map(_/1000000).stdlib.mkString(" ms, ")+" ms")
