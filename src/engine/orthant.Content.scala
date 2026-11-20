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
    var result: scala.List[Text] = scala.Nil
    var index = to.min(array.length) - 1
    val first = from.max(0)

    while index >= first do
      result = array(index) :: result
      index -= 1

    List.from(result)

  // Each distinct line is a symbol, numbered from zero.
  private val symbols: java.util.HashMap[Text, Integer] = java.util.HashMap()

  private val ids: scala.Array[Int] = array.map: line =>
    val known = symbols.get(line)

    if known != null then known.intValue else
      val next = symbols.size
      symbols.put(line, Integer.valueOf(next))
      next

  private def symbol(line: Text): Int =
    val known = symbols.get(line)
    if known == null then -1 else known.intValue

  private lazy val forward: Content.Automaton = Content.Automaton(ids)
  private lazy val backward: Content.Automaton = Content.Automaton(ids.reverse)

  private def matches(block: List[Text], index: Int): Boolean =
    var todo = block.stdlib
    var position = index
    var same = position >= 0

    while same && !todo.isEmpty do
      same = position < array.length && array(position) == todo.head
      todo = todo.tail
      position += 1

    same

  // Where a run of lines occurs: the position it begins at if it occurs once, `Content.Absent`
  // if it does not occur, and `Content.Several` if it occurs more than once. A run which must be
  // at the start or the end of the content occurs once at most.
  def locate(block: List[Text], start: Boolean = false, end: Boolean = false): Int =
    val length = block.stdlib.length

    if start then
      if (!end || length == array.length) && matches(block, 0) then 0 else Content.Absent
    else if end then
      if matches(block, array.length - length) then array.length - length else Content.Absent
    else if length == 0 then (if array.length == 0 then 0 else Content.Several)
    else
      var state = 0
      var todo = block.stdlib

      while state >= 0 && !todo.isEmpty do
        val id = symbol(todo.head)
        state = if id < 0 then -1 else forward.step(state, id)
        todo = todo.tail

      if state < 0 then Content.Absent
      else if forward.count(state) > 1 then Content.Several
      else forward.first(state) - length + 1

  // The fewest lines before `start` which, with the `length` lines from `start`, make a run that
  // occurs once in the content; `Unset` if even every line before it is not enough. An empty run
  // is not accepted, nor, if `context` is set, one which has no line before `start`.
  def anchor(start: Int, length: Int, context: Boolean): Optional[Int] =
    var state = 0
    var index = start + length - 1

    while state >= 0 && index >= start do
      state = backward.step(state, ids(index))
      index -= 1

    def unique: Boolean = state > 0 && backward.count(state) == 1
    var count = 0
    var found = !context && unique

    while !found && state >= 0 && count < start do
      count += 1
      state = backward.step(state, ids(start - count))
      found = unique

    if found then count else Unset

object Content:
  val Absent: Int = -1
  val Several: Int = -2

  // The suffix automaton of a sequence of symbols. Each state stands for the runs which end at
  // the same set of positions: `count` is how many positions those are, and `first` the earliest.
  final class Automaton(symbols: scala.Array[Int]):
    private val limit = symbols.length*2 + 1
    private val length = new scala.Array[Int](limit)
    private val link = new scala.Array[Int](limit)
    private val counts = new scala.Array[Int](limit)
    private val firsts = new scala.Array[Int](limit)
    private val edges = new scala.Array[java.util.HashMap[Integer, Integer]](limit)
    private var states = 1
    private var last = 0

    link(0) = -1
    edges(0) = java.util.HashMap()

    private def target(state: Int, symbol: Int): Int =
      val found = edges(state).get(Integer.valueOf(symbol))
      if found == null then -1 else found.intValue

    private def extend(symbol: Int, position: Int): Unit =
      val current = states
      states += 1
      length(current) = length(last) + 1
      counts(current) = 1
      firsts(current) = position
      edges(current) = java.util.HashMap()
      var state = last

      while state >= 0 && target(state, symbol) < 0 do
        edges(state).put(Integer.valueOf(symbol), Integer.valueOf(current))
        state = link(state)

      if state < 0 then link(current) = 0 else
        val next = target(state, symbol)

        if length(state) + 1 == length(next) then link(current) = next else
          val clone = states
          states += 1
          length(clone) = length(state) + 1
          edges(clone) = java.util.HashMap(edges(next))
          link(clone) = link(next)
          firsts(clone) = firsts(next)

          while state >= 0 && target(state, symbol) == next do
            edges(state).put(Integer.valueOf(symbol), Integer.valueOf(clone))
            state = link(state)

          link(next) = clone
          link(current) = clone

      last = current

    private def build(): Unit =
      var position = 0

      while position < symbols.length do
        extend(symbols(position), position)
        position += 1

      // A state's positions are its own and those of every state which links to it: the states
      // are taken longest first, by a counting sort on length.
      val buckets = new scala.Array[Int](symbols.length + 2)
      var state = 0

      while state < states do
        buckets(length(state) + 1) += 1
        state += 1

      var size = 1
      while size < buckets.length do
        buckets(size) += buckets(size - 1)
        size += 1

      val order = new scala.Array[Int](states)
      state = 0

      while state < states do
        order(buckets(length(state))) = state
        buckets(length(state)) += 1
        state += 1

      var index = states - 1

      while index > 0 do
        val each = order(index)
        counts(link(each)) += counts(each)
        index -= 1

    build()

    // The state reached from `state` by `symbol`, or -1 if no run continues so.
    def step(state: Int, symbol: Int): Int = target(state, symbol)
    def count(state: Int): Int = counts(state)

    // The position of the last symbol of the earliest occurrence of the state's runs.
    def first(state: Int): Int = firsts(state)
