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

  // Performs `action` on each index from `from` up to, not including, `to`.
  @tailrec
  private def ascend(from: Int, to: Int)(action: Int => Unit): Unit =
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
