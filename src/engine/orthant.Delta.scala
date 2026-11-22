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

import scala.annotation.tailrec

import proscenium.{List, Nil, `::`}
import soundness.*

// The text of a delta file (`spec/store.md` §8): a header naming the format version and the
// storage axis, then hunks, each after a blank line, every line of which begins with a sigil.
// A hunk's anchor is one line: how many lines are before the lines it replaces, how many it
// replaces and how many are after, and the imprint of them all.
object Delta:
  val version: Int = 2

  // The version in which a hunk's anchor was written out, line by line.
  val legacy: Int = 1

  enum Reason:
    case Header, Sigil, Order, Version

  case class Error(line: Int, reason: Reason)(using Diagnostics)
  extends fulminate.Error
    ( reason match
        case Reason.Header  => m"line $line is not the header of a delta file"
        case Reason.Sigil   => m"line $line of the delta file does not begin with a sigil"
        case Reason.Order   => m"line $line of the delta file is out of order within its hunk"
        case Reason.Version => m"the delta file is of another format version" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Header  => Orthant.bolt(1, 1)
      case Reason.Sigil   => Orthant.bolt(1, 2)
      case Reason.Order   => Orthant.bolt(1, 3)
      case Reason.Version => Orthant.bolt(1, 4)

  private def mark(sigil: Text, line: Text): Text = if line == t"" then sigil else t"$sigil $line"

  def serialize(record: Folio.Delta | Folio.Absent): List[Text] = record match
    case Folio.Absent(axis) =>
      List(t"orthant $version $axis absent")

    case Folio.Delta(axis, hunks) =>
      t"orthant $version $axis" :: hunks.bind: hunk =>
        val sigil = if hunk.kept then t"%" else t"@"
        val anchor = t"$sigil ${hunk.before} ${hunk.replaced} ${hunk.after} ${hunk.anchor.text}"

        List[List[Text]]
          ( List(t""),
            if hunk.start then List(t"^") else Nil,
            hunk.former.map(mark(t"!", _)),
            List(anchor),
            if hunk.end then List(t"$$") else Nil,
            hunk.lines.map(mark(t"+", _)) )

        . bind: lines => lines

  // The stages of a hunk, in the order its lines must come: `^`, former lines, the anchor, `$`,
  // new lines.
  private def stage(sigil: Text): Optional[Int] = sigil match
    case t"^"  => 0
    case t"!"  => 1
    case t"@"  => 2
    case t"%"  => 2
    case t"$$" => 3
    case t"+"  => 4
    case _     => Unset

  // A count is read a digit at a time: there is one for every hunk of a store, three times.
  private def count(text: Text): Optional[Int] =
    val digits = text.s
    val size = digits.length

    @tailrec
    def recur(index: Int, number: Int): Optional[Int] =
      if index == size then number else
        val digit = digits.charAt(index) - '0'
        if digit < 0 || digit > 9 then Unset else recur(index + 1, number*10 + digit)

    if size == 0 || size > 9 || size > 1 && digits.charAt(0) == '0' then Unset else recur(0, 0)

  private val blank: Sealed = Sealed(0, 0, 0, Imprint.empty, Nil)

  private def header(text: List[Text], version: Int): (Text, Boolean, List[Text]) raises Error =
    text match
      case header :: tail => header.cut(t" ") match
        case List(t"orthant", number, axis) =>
          if number != version.show then abort(Error(1, Reason.Version))
          (axis, false, tail)

        case List(t"orthant", number, axis, t"absent") =>
          if number != version.show then abort(Error(1, Reason.Version))
          (axis, true, tail)

        case _ =>
          abort(Error(1, Reason.Header))

      case _ =>
        abort(Error(1, Reason.Header))

  def parse(text: List[Text]): Folio.Delta | Folio.Absent raises Error =
    // `stage0` is the stage of the hunk's last line, or -1 between hunks; a hunk is whole only
    // once it has had its anchor.
    def hunks(todo: List[Text], number: Int, stage0: Int, hunk: Sealed, done: List[Sealed])
    :   List[Sealed] =

      def close: List[Sealed] =
        if stage0 < 0 then done
        else if stage0 < 2 then abort(Error(number, Reason.Order))
        else hunk.copy(lines = hunk.lines.reverse, former = hunk.former.reverse) :: done

      todo match
        case t"" :: tail =>
          hunks(tail, number + 1, -1, blank, close)

        case line :: tail =>
          val sigil = line.keep(1)
          val content = line.skip(2)
          if line.length > 1 && line.keep(2) != t"$sigil " then abort(Error(number, Reason.Sigil))
          val next = stage(sigil).or(abort(Error(number, Reason.Sigil)))

          if next < stage0 || next == stage0 && next != 1 && next != 4
          then abort(Error(number, Reason.Order))

          // The end mark and the new lines come after the anchor, which every hunk has.
          if next > 2 && stage0 < 2 then abort(Error(number, Reason.Order))

          if (next == 0 || next == 3) && line.length > 1 then abort(Error(number, Reason.Sigil))

          // A kept hunk's lines are those it replaces, and it has no others.
          if next == 4 && hunk.kept then abort(Error(number, Reason.Order))

          val hunk2 = next match
            case 0 => hunk.copy(start = true)
            case 1 => hunk.copy(former = content :: hunk.former)
            case 3 => hunk.copy(end = true)
            case 4 => hunk.copy(lines = content :: hunk.lines)

            case _ => content.cut(t" ") match
              case List(before, replaced, after, anchor) =>
                def number2(text: Text): Int = count(text).or(abort(Error(number, Reason.Sigil)))
                val imprint = Imprint.read(anchor).or(abort(Error(number, Reason.Sigil)))

                hunk.copy
                  ( before   = number2(before),
                    replaced = number2(replaced),
                    after    = number2(after),
                    anchor   = imprint,
                    kept     = sigil == t"%" )

              case _ =>
                abort(Error(number, Reason.Sigil))

          hunks(tail, number + 1, next, hunk2, done)

        case _ =>
          close.reverse

    val (axis, absent, tail) = header(text, version)
    if absent then Folio.Absent(axis) else Folio.Delta(axis, hunks(tail, 2, -1, blank, Nil))

  // The stages of a hunk of the version before: `^`, context, former lines, replaced lines,
  // context, `$`, new lines.
  private def former(sigil: Text, current: Int): Optional[Int] = sigil match
    case t"^"  => 0
    case t"="  => if current <= 1 then 1 else if current <= 4 then 4 else Unset
    case t"!"  => 2
    case t"-"  => 3
    case t"$$" => 5
    case t"+"  => 6
    case _     => Unset

  // The record which a delta file of the version before held, in which each line of a hunk's
  // anchor was written out: `=` before a line of context, and `-` before a replaced line.
  def upgrade(text: List[Text]): Folio.Delta | Folio.Absent raises Error =
    def hunks(todo: List[Text], number: Int, stage0: Int, hunk: Hunk, done: List[Hunk])
    :   List[Hunk] =

      def close: List[Hunk] = if stage0 < 0 then done else hunk :: done

      todo match
        case t"" :: tail =>
          hunks(tail, number + 1, -1, Hunk(Nil, Nil), close)

        case line :: tail =>
          val sigil = line.keep(1)
          val content = line.skip(2)
          if line.length > 1 && line.keep(2) != t"$sigil " then abort(Error(number, Reason.Sigil))
          val next = former(sigil, stage0).or(abort(Error(number, Reason.Sigil)))
          if next < stage0 then abort(Error(number, Reason.Order))
          if (next == 0 || next == 5) && line.length > 1 then abort(Error(number, Reason.Sigil))

          val hunk2 = next match
            case 0 => hunk.copy(start = true)
            case 1 => hunk.copy(before = content :: hunk.before)
            case 2 => hunk.copy(former = content :: hunk.former)
            case 3 => hunk.copy(replaced = content :: hunk.replaced)
            case 4 => hunk.copy(after = content :: hunk.after)
            case 5 => hunk.copy(end = true)
            case _ => hunk.copy(lines = content :: hunk.lines)

          hunks(tail, number + 1, next, hunk2, done)

        case _ =>
          close.reverse

    def tidy(hunk: Hunk): Hunk =
      hunk.copy
        ( before   = hunk.before.reverse,
          former   = hunk.former.reverse,
          replaced = hunk.replaced.reverse,
          after    = hunk.after.reverse,
          lines    = hunk.lines.reverse )

    val (axis, absent, tail) = header(text, legacy)

    if absent then Folio.Absent(axis)
    else Folio.Delta(axis, hunks(tail, 2, -1, Hunk(Nil, Nil), Nil).map(tidy(_).seal))
