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

import proscenium.List
import soundness.*

// The hash by which a run of lines is known where the lines themselves are not kept
// (`spec/store.md` §8.4). It is two numbers below the prime 2⁶¹ − 1, each made the same way
// from constants of its own: a line's number is made of its bytes, and a run's of its lines'
// numbers, each a polynomial in a base, so that the number of every run of a content is had
// from the numbers of the runs which begin it.
object Imprint:
  final val Prime: Long = (1L << 61) - 1

  // The bases: of a line's bytes, and of a run's lines, for each of the two numbers.
  final val Byte1: Long = 0x043f6a8885a308d3L
  final val Byte2: Long = 0x13198a2e03707344L
  final val Line1: Long = 0x04093822299f31d0L
  final val Line2: Long = 0x082efa98ec4e6c89L

  val empty: Imprint = Imprint(0L, 0L)

  private def reduce(value: Long): Long =
    val folded = (value & Prime) + (value >>> 61)
    if folded >= Prime then folded - Prime else folded

  // The product of two numbers below the prime, modulo the prime.
  def times(left: Long, right: Long): Long =
    val high = Math.multiplyHigh(left, right)
    val low = left*right
    reduce((low & Prime) + ((high << 3) | (low >>> 61)))

  // What a number becomes when one more thing, itself a number, comes after what it was made of.
  def step(number: Long, base: Long, next: Long): Long =
    reduce(times(number, base) + reduce(next + 1))

  private def bytes(text: Text, base: Long): Long =
    val data = text.s.getBytes(java.nio.charset.StandardCharsets.UTF_8).nn

    @tailrec
    def recur(index: Int, number: Long): Long =
      if index == data.length then number
      else recur(index + 1, step(number, base, (data(index) & 0xff).toLong))

    recur(0, 0L)

  // The two numbers of a line.
  def line(text: Text): Imprint = Imprint(bytes(text, Byte1), bytes(text, Byte2))

  def of(lines: List[Text]): Imprint =
    lines.fold(empty): (imprint, text) =>
      val next = line(text)
      Imprint(step(imprint.first, Line1, next.first), step(imprint.second, Line2, next.second))

  // A imprint as it is written: thirty-two hexadecimal digits, sixteen for each number.
  def read(text: Text): Optional[Imprint] =
    val digits = text.s

    // The number which the sixteen digits from `from` give, or -1 if one is no digit: a
    // number below the prime is not negative.
    @tailrec
    def number(index: Int, to: Int, value: Long): Long =
      if index == to then value else
        val char = digits.charAt(index)

        val digit =
          if char >= '0' && char <= '9' then char - '0'
          else if char >= 'a' && char <= 'f' then char - 'a' + 10
          else -1

        if digit < 0 then -1L else number(index + 1, to, (value << 4) | digit)

    if digits.length != 32 then Unset else
      val first = number(0, 16, 0L)
      val second = number(16, 32, 0L)
      if first < 0 || second < 0 then Unset else Imprint(first, second)

case class Imprint(first: Long, second: Long):
  def text: Text = String.format("%016x%016x", first, second).nn.tt
