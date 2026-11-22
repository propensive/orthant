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

object Content:
  val Absent: Int = -1
  val Several: Int = -2

  // The boxed form of `Absent`, which a lookup in a Java map gives for a key it lacks.
  private val Missing: Integer = Integer.valueOf(Absent).nn

  // The least index below `size` for which `test` holds, where it holds for every index after
  // the first for which it does; `size` if it holds for none.
  def search(size: Int)(test: Int => Boolean): Int =
    @tailrec
    def recur(low: Int, high: Int): Int =
      if low >= high then low else
        val middle = (low + high)/2
        if test(middle) then recur(low, middle) else recur(middle + 1, high)

    recur(0, size)

  // Performs `action` on each index from `from` up to, not including, `to`.
  @tailrec
  def ascend(from: Int, to: Int)(action: Int => Unit): Unit =
    if from < to then
      action(from)
      ascend(from + 1, to)(action)

  // The suffix automaton of a sequence of symbols. Each state stands for the runs which end at
  // the same set of positions: `count` is how many positions those are, and `first` the earliest.
  final class Automaton(symbols: scala.Array[Int]):
    private val limit = symbols.length*2 + 1
    private val length = new scala.Array[Int](limit)
    private val link = new scala.Array[Int](limit)
    private val counts = new scala.Array[Int](limit)
    private val firsts = new scala.Array[Int](limit)
    private val edges = new scala.Array[java.util.HashMap[Integer, Integer]](limit)

    link(0) = -1
    edges(0) = java.util.HashMap()

    private def target(state: Int, symbol: Int): Int =
      edges(state).getOrDefault(Integer.valueOf(symbol), Missing).nn.intValue

    // From `state` along its links, each state whose edge on `symbol` leads to `from` is made to
    // lead to `to`; the result is the first state which does not, or -1.
    @tailrec
    private def redirect(state: Int, symbol: Int, from: Int, to: Int): Int =
      if state < 0 || target(state, symbol) != from then state else
        edges(state).put(Integer.valueOf(symbol), Integer.valueOf(to))
        redirect(link(state), symbol, from, to)

    // The automaton of `states` states, the whole sequence so far ending at `last`, extended by
    // the symbol at `position`; the result is how many states it then has.
    private def extend(symbol: Int, position: Int, states: Int, last: Int): Int =
      val current = states
      length(current) = length(last) + 1
      counts(current) = 1
      firsts(current) = position
      edges(current) = java.util.HashMap()
      val state = redirect(last, symbol, Absent, current)

      if state < 0 then
        link(current) = 0
        states + 1
      else
        val next = target(state, symbol)

        if length(state) + 1 == length(next) then
          link(current) = next
          states + 1
        else
          val clone = states + 1
          length(clone) = length(state) + 1
          edges(clone) = java.util.HashMap(edges(next))
          link(clone) = link(next)
          firsts(clone) = firsts(next)
          redirect(state, symbol, next, clone)
          link(next) = clone
          link(current) = clone
          states + 2

    @tailrec
    private def grow(position: Int, states: Int, last: Int): Int =
      if position == symbols.length then states
      else grow(position + 1, extend(symbols(position), position, states, last), states)

    private val states: Int = grow(0, 1, 0)

    // A state's positions are its own and those of every state which links to it: the states
    // are taken longest first, by a counting sort on length.
    private def tally(): Unit =
      val buckets = new scala.Array[Int](symbols.length + 2)
      ascend(0, states): state => buckets(length(state) + 1) += 1
      ascend(1, buckets.length): size => buckets(size) += buckets(size - 1)
      val order = new scala.Array[Int](states)

      ascend(0, states): state =>
        order(buckets(length(state))) = state
        buckets(length(state)) += 1

      ascend(1, states): index =>
        val each = order(states - index)
        counts(link(each)) += counts(each)

    tally()

    // The state reached from `state` by `symbol`, or -1 if no run continues so.
    def step(state: Int, symbol: Int): Int = target(state, symbol)
    def count(state: Int): Int = counts(state)

    // The position of the last symbol of the earliest occurrence of the state's runs.
    def first(state: Int): Int = firsts(state)

// The lines of a document, held so that any of them is reached at once and so that the places a
// run of lines occurs are found in time proportional to the run, however long the document is.
// A run is sought through a suffix automaton of the lines, each line standing as one symbol:
// the automaton is built once, in time linear in the document, when first it is wanted.
final class Content(val lines: List[Text]):
  private val array: scala.Array[Text] = lines.stdlib.toArray
  def size: Int = array.length
  def apply(index: Int): Text = array(index)

  // The lines from `from` up to, not including, `to`.
  def slice(from: Int, to: Int): List[Text] =
    val first = from.max(0)

    @tailrec
    def gather(index: Int, result: scala.List[Text]): scala.List[Text] =
      if index < first then result else gather(index - 1, array(index) :: result)

    List.from(gather(to.min(array.length) - 1, scala.Nil))

  // Each distinct line is a symbol, numbered from zero.
  private val symbols: java.util.HashMap[Text, Integer] = java.util.HashMap()

  private val ids: scala.Array[Int] = array.map: line =>
    val known = symbol(line)

    if known >= 0 then known else
      val next = symbols.size
      symbols.put(line, Integer.valueOf(next))
      next

  private def symbol(line: Text): Int = symbols.getOrDefault(line, Content.Missing).nn.intValue

  // The imprint of the run of lines which begins the content, for each number of lines, as its
  // two numbers; and each power of the two bases. The imprint of a line is found once for each
  // distinct line. They are made when first a imprint is wanted.
  private final class Marks:
    val first = new scala.Array[Long](array.length + 1)
    val second = new scala.Array[Long](array.length + 1)
    val powers1 = new scala.Array[Long](array.length + 1)
    val powers2 = new scala.Array[Long](array.length + 1)
    private val known = new scala.Array[Imprint | Null](symbols.size)
    powers1(0) = 1L
    powers2(0) = 1L

    Content.ascend(0, array.length): index =>
      val line: Imprint = Optional(known(ids(index))).or:
        val made = Imprint.line(array(index))
        known(ids(index)) = made
        made

      first(index + 1) = Imprint.step(first(index), Imprint.Line1, line.first)
      second(index + 1) = Imprint.step(second(index), Imprint.Line2, line.second)
      powers1(index + 1) = Imprint.times(powers1(index), Imprint.Line1)
      powers2(index + 1) = Imprint.times(powers2(index), Imprint.Line2)

  private lazy val marks: Marks = Marks()

  private def window(marks: scala.Array[Long], powers: scala.Array[Long], from: Int, to: Int)
  :   Long =

    val rest = marks(to) - Imprint.times(marks(from), powers(to - from))
    if rest < 0 then rest + Imprint.Prime else rest

  // The imprint of the lines from `from` up to, not including, `to`.
  def imprint(from: Int, to: Int): Imprint =
    Imprint
      ( window(marks.first, marks.powers1, from, to),
        window(marks.second, marks.powers2, from, to) )

  // For a number of lines, where each run of so many begins, by its imprint: `Content.Several`
  // for an imprint which more than one run has. It is made when first a run of that length is
  // sought, in time linear in the document, and holds nothing but numbers: a slot for each run
  // and as many again, each run in the first free slot from the one its imprint gives.
  private final class Runs(length: Int):
    private val count = array.length - length + 1
    private val mask = Integer.highestOneBit(count.max(1))*4 - 1
    private val firsts = new scala.Array[Long](mask + 1)
    private val seconds = new scala.Array[Long](mask + 1)
    private val places = new scala.Array[Int](mask + 1)

    // The slot which holds the imprint, or the free one in which it would be put: a place is
    // kept as one more than it is, so that a free slot has nought.
    @tailrec
    private def slot(first: Long, second: Long, index: Int): Int =
      if places(index) == 0 || firsts(index) == first && seconds(index) == second then index
      else slot(first, second, (index + 1) & mask)

    private def slot(first: Long, second: Long): Int =
      slot(first, second, ((first ^ (first >>> 29) ^ second).toInt*0x9e3779b1) & mask)

    Content.ascend(0, count): index =>
      val run = imprint(index, index + length)
      val found = slot(run.first, run.second)
      firsts(found) = run.first
      seconds(found) = run.second
      places(found) = if places(found) == 0 then index + 1 else Content.Several + 1

    def apply(sought: Imprint): Int =
      val place = places(slot(sought.first, sought.second))
      if place == 0 then Content.Absent else place - 1

  private val runs: java.util.HashMap[Integer, Runs] = java.util.HashMap()

  private def table(length: Int): Runs =
    Optional(runs.get(Integer.valueOf(length))).or:
      val made = Runs(length)
      runs.put(Integer.valueOf(length), made)
      made

  // Where a run of `length` lines which has the imprint occurs: the position it begins at if
  // there is one such, `Content.Absent` if there is none, and `Content.Several` if more than
  // one. A run which must be at the start or the end of the content occurs once at most.
  def find(length: Int, sought: Imprint, start: Boolean = false, end: Boolean = false): Int =
    def at(index: Int): Int =
      if index >= 0 && index + length <= array.length && imprint(index, index + length) == sought
      then index
      else Content.Absent

    if start then (if !end || length == array.length then at(0) else Content.Absent)
    else if end then at(array.length - length)
    else if length == 0 then (if array.length == 0 then 0 else Content.Several)
    else if length > array.length then Content.Absent
    else table(length)(sought)

  private lazy val forward: Content.Automaton = Content.Automaton(ids)
  private lazy val backward: Content.Automaton = Content.Automaton(ids.reverse)

  private def matches(block: List[Text], index: Int): Boolean =
    @tailrec
    def recur(todo: scala.List[Text], position: Int): Boolean =
      if todo.isEmpty then true
      else if position >= array.length || array(position) != todo.head then false
      else recur(todo.tail, position + 1)

    index >= 0 && recur(block.stdlib, index)

  // Where a run of lines occurs: the position it begins at if it occurs once, `Content.Absent`
  // if it does not occur, and `Content.Several` if it occurs more than once. A run which must be
  // at the start or the end of the content occurs once at most.
  def locate(block: List[Text], start: Boolean = false, end: Boolean = false): Int =
    val length = block.stdlib.length

    if start then
      if (!end || length == array.length) && matches(block, 0) then 0 else Content.Absent
    else if end then
      if matches(block, array.length - length) then array.length - length else Content.Absent
    else if length == 0 then
      if array.length == 0 then 0 else Content.Several
    else
      val state = block.fold(0): (state, line) =>
        if state < 0 then state else
          val id = symbol(line)
          if id < 0 then -1 else forward.step(state, id)

      if state < 0 then Content.Absent
      else if forward.count(state) > 1 then Content.Several
      else forward.first(state) - length + 1

  // The fewest lines before `start` which, with the `length` lines from `start`, make a run that
  // occurs once in the content; `Unset` if even every line before it is not enough. An empty run
  // is not accepted, nor, if `context` is set, one which has no line before `start`.
  def anchor(start: Int, length: Int, context: Boolean): Optional[Int] =
    def unique(state: Int): Boolean = state > 0 && backward.count(state) == 1

    @tailrec
    def run(state: Int, index: Int): Int =
      if state < 0 || index < start then state else run(backward.step(state, ids(index)), index - 1)

    // The state of the run with `count` lines before it, which is not unique, widened by more.
    @tailrec
    def widen(state: Int, count: Int): Optional[Int] =
      if state < 0 || count >= start then Unset else
        val next = backward.step(state, ids(start - count - 1))
        if unique(next) then count + 1 else widen(next, count + 1)

    val initial = run(0, start + length - 1)
    if !context && unique(initial) then 0 else widen(initial, 0)
