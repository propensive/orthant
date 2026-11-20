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
import soundness.{Holding as _, *}
import soundness.dysasymptotics.linearSize
import soundness.stdios.fileDescriptorStdio
import soundness.termcapDefinitions.basicTermcap

// Prints the first invariant each run of random check-ins breaks, or, given a seed, the store as
// it was before the failing step and what that step did:
// `java -cp <test jar> orthant.probe [seed]`; `orthant.probe -500` runs 500 seeds.
@main
def probe(seeds: Int*): Unit =
  import strategies.throwUnsafely
  import Trials.*

  def show(lines: Optional[List[Text]]): Text = lines.let(_.join(t"|")).or(t"(absent)")

  def dump(dossier: Dossier, only: Point => Boolean): Unit = points.each: point =>
    if only(point) then
      val issues = dossier.issues(point).size
      Out.println(t"  ${name(point)}: ${show(dossier.content(point))}  [$issues issues]")

      dossier.rendition(point).let: rendition =>
        Out.println(t"      on ${show(rendition.reference)}")

        rendition.placements.each: placement =>
          Out.println:
            t"      @${placement.start}-${placement.end} from ${name(placement.origin)}" +
              t" ${if placement.fresh then t"fresh" else t"stale"} ${show(placement.lines)}"

      dossier.folios.at(point).let:
        case delta: Folio.Delta =>
          Delta.serialize(delta).each: line => Out.println(t"      $line")

        case Folio.Base(lines)    => Out.println(t"      base ${show(lines)}")
        case absent: Folio.Absent => Out.println(t"      absent")

  if seeds.isEmpty then List.range(1, 41).each: seed =>
    Out.println(t"$seed: ${trial(seed, 25).or(t"ok")}")

  // A negative number runs that many seeds, at greater length, and prints only the failures.
  seeds.filter(_ < 0).foreach: count =>
    List.range(1, 1 - count).each: seed =>
      trial(seed, 40).let: failure => Out.println(t"$seed: $failure")

  seeds.filter(_ > 0).foreach: seed =>
    def recur(dossier: Dossier, step: Int): Unit =
      val (after, failure) = check(dossier, seed, step)

      failure.lay(if step < 39 then recur(after, step + 1)): failure =>
        val Step(point, lines, scope, label) = plan(dossier, seed, step)
        def related(other: Point): Boolean = space.below(other, point) || space.below(point, other)
        Out.println(t"$failure")
        Out.println(t"wanted ${show(lines)}")
        Out.println(t"before:")
        dump(dossier, related)
        Out.println(t"stored alone:")
        dump(dossier.store(point, lines), related)

        safely(dossier.checkin(point, lines, scope)).let: once =>
          once.folios.values.each:
            case delta: Folio.Delta if Delta.parse(Delta.serialize(delta)) != delta =>
              Out.println(t"not written as read: ${delta.toString}")
              Delta.serialize(delta).each: line => Out.println(t"      $line")

            case _ =>
              ()

          val twice = once.checkin(point, lines, scope)
          def differs(other: Point): Boolean = once.folios.at(other) != twice.folios.at(other)
          Out.println(t"checked in once, where a second differs:")
          dump(once, differs)
          Out.println(t"checked in twice:")
          dump(twice, differs)

    recur(start, 0)

// Runs every invariant over the seeds from `from` up to `to`, and prints the first few which
// each kind of run breaks: `java -cp <test jar> orthant.sweep 1 500`.
@main
def sweep(from: Int, to: Int): Unit =
  import strategies.throwUnsafely

  def report(name: Text, failures: List[Optional[Text]]): Unit =
    val found = failures.filter(_.present)
    Out.println(t"$name: ${found.count { failure => true }} failures")
    found.excerpt(0, 12).each: failure => Out.println(t"  ${failure.or(t"")}")

  val seeds = List.range(from, to)

  def named(seed: Int, failure: Optional[Text]): Optional[Text] =
    failure.let: failure => t"seed $seed: $failure"

  report(t"trial", seeds.map { seed => named(seed, Trials.trial(seed, 30)) })
  report(t"lawful", seeds.map { seed => named(seed, Trials.lawful(seed, 30)) })
  report(t"solo", seeds.map { seed => Solo.trial(seed, 40) })
