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
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.systems.javaBaseSystem

// Times the reading of a store on disk, the derivation of every document at every point, and
// the pending lines of each: `java -cp <test jar> orthant.timing <store's home>`.
@main
def timing(root: String): Unit =
  import strategies.throwUnsafely
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)
  val start = now
  val store = Store.inspect(Disk.path(root.tt))
  say("read "+store.documents.size.toString+" documents in "+since(start))

  store.points.each: point =>
    val begin = now

    val lines = store.documents.map: document =>
      store.dossier(document).content(point).let(_.size).or(0)

    val issues = store.documents.map: document => store.dossier(document).issues(point).size
    val pending = store.documents.map: document => store.dossier(document).pending(point).size
    val total = lines.fold(0)(_ + _).toString+" lines, "+issues.fold(0)(_ + _).toString+" issues, "

    say(store.name(point).s+": "+total+pending.fold(0)(_ + _).toString+" pending in "+since(begin))

  val check = now
  say("faults: "+store.faults.size.toString+" in "+since(check))
  say("total "+since(start))

// Times a check-in of each of the longest documents of a store on disk, unchanged and with one
// line changed, at every point: `java -cp <test jar> orthant.timingCheckin <store's home> <count>`.
@main
def timingCheckin(root: String, count: Int): Unit =
  import strategies.throwUnsafely
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)
  val store = Store.inspect(Disk.path(root.tt))
  val last = store.points.last.or(panic(m"a store has a point"))

  val longest: List[Text] =
    store.documents.map: document =>
      (store.dossier(document).content(last).let(_.size).or(0), document)

    . stdlib.sortBy(-_(0)).take(count).map(_(1)).pipe(List.from(_))

  longest.each: document =>
    store.points.each: point =>
      val dossier = store.dossier(document)

      dossier.content(point).let: lines =>
        val begin = now
        val same = dossier.checkin(point, lines)
        val first = since(begin)
        val unchanged = same.folios == dossier.folios
        val edited = lines.stdlib.updated(lines.size/2, t"a changed line").pipe(List.from(_))
        val again = now
        val changed = dossier.checkin(point, edited)
        val ok = changed.content(point) == edited

        val where = document.s+" at "+store.name(point).s+" ("+lines.size.toString+" lines)"
        val outcome = " (same records: "+unchanged.toString+"), one line changed "+since(again)
        say(where+": unchanged "+first+outcome+" (ok: "+ok.toString+")")

// Does what `check`, `export` at every point and `status` at every point do, on a store on disk,
// and times each: `java -cp <test jar> orthant.timingCommands <store's home> <directory>`.
@main
def timingCommands(root: String, target: String): Unit =
  import strategies.throwUnsafely
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)
  val start = now
  val store = Workspace.load(root.tt)
  say("loaded and checked "+store.documents.size.toString+" documents in "+since(start))

  store.space.points.each: point =>
    val begin = now
    val count = Worktree.publish(store, point, target.tt+t"/"+store.name(point), false)
    val exported = since(begin)
    val again = now
    val sum = Survey.sum(store, point)

    val exports = store.name(point).s+": exported "+count.toString+" in "+exported+"; "
    val faults = sum.stale.toString+" stale, "+sum.faults.toString+" faults in "+since(again)
    say(exports+sum.pending.toString+" pending, "+faults)

  say("total "+since(start))

// Prints a document of a store on disk at the point whose directory is named, `root` for the
// root: `java -cp <test jar> orthant.catDocument <store's home> <document> <point>`.
@main
def catDocument(root: String, document: String, name: String): Unit =
  import strategies.throwUnsafely
  val store = Store.inspect(Disk.path(root.tt))

  val point =
    store.points.filter { point => store.name(point) == name.tt }.head.or(panic(m"no such point"))

  val lines = store.dossier(document.tt).content(point).or(Nil)
  java.lang.System.out.nn.print(Store.text(lines).s)

// Checks documents in at the point whose directory is named, each from a file of its content:
// `java -cp <test jar> orthant.checkinFiles <store's home> <point> <directory> <document>…`,
// where the content of each document is the file of its name in the directory.
@main
def checkinFiles(root: String, name: String, directory: String, documents: String*): Unit =
  import strategies.throwUnsafely
  val loaded = Workspace.load(root.tt)

  val point =
    loaded.space.points.filter { point => loaded.name(point) == name.tt }.head
    . or(panic(m"no such point"))

  documents.foldLeft(loaded): (store, document) =>
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(directory, document))
    val text = new String(bytes, "UTF-8")
    val lines = Store.lines(text.tt)
    val dossier = store.dossier(document.tt).checkin(point, lines)
    java.lang.System.out.nn.println("checked in "+document)
    store.save(document.tt, dossier)

// Prints the pending lines and the stale passages of each document at the point whose directory
// is named: `java -cp <test jar> orthant.workAt <store's home> <point>`.
@main
def workAt(root: String, name: String): Unit =
  import strategies.throwUnsafely
  val store = Workspace.load(root.tt)

  val point =
    store.space.points.filter { point => store.name(point) == name.tt }.head
    . or(panic(m"no such point"))

  store.documents.each: document =>
    val work = Survey.work(store, document, point)
    val counts = work.pending.size.toString+"\t"+work.stale.size.toString+"\t"+work.faults.toString
    if work.wanting then java.lang.System.out.nn.println(document.s+"\t"+counts)

// Times reading a repository's references and asking, of every pair of tags, whether one is an
// ancestor of the other: `java -cp <test jar> orthant.timingSync <repository>`.
@main
def timingSync(root: String): Unit =
  import strategies.throwUnsafely
  val start = java.lang.System.nanoTime

  Sync.reading(root.tt): refs =>
    val read = java.lang.System.nanoTime
    val hashes = refs.tags.map(_(1))

    val count = hashes.fold(0): (count, upper) =>
      count + hashes.filter(refs.ancestor(upper, _)).size

    val asked = java.lang.System.nanoTime

    java.lang.System.out.nn.println
      ( s"${refs.tags.size} tags, ${refs.branches.size} branches; read in " +
        s"${(read - start)/1000000} ms; ${hashes.size*hashes.size} questions, $count yes, in " +
        s"${(asked - read)/1000000} ms" )

// Times finding everything wrong with a store on disk, with its documents derived in turn and
// then in tasks of their own: `java -cp <test jar> orthant.timingTasks <store's home>`.
@main
def timingTasks(root: String): Unit =
  import strategies.throwUnsafely
  import threads.virtualThreads
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"

  List.range(0, 4).each: round =>
    val first = Workspace.inspect(root.tt)
    val begin = now
    val count = first.faults.size
    val serial = since(begin)
    val second = Workspace.inspect(root.tt)
    val again = now
    supervise(second.prepare(second.points))
    val same = second.faults.size == count
    val tasks = since(again)
    java.lang.System.out.nn.println("in turn "+serial+"; in tasks "+tasks+"; same: "+same.toString)

// Times what a command does first on a store with a sourced axis: opening it, twice, asking
// whether each value's commit descends from its parent's, and placing each of the revisions
// given among the values, twice:
// `java -cp <test jar> orthant.timingSourced <store's home> <revision>…`.
@main
def timingSourced(root: String, revisions: String*): Unit =
  import strategies.throwUnsafely
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)
  val start = now
  val store = Workspace.open(root.tt)
  say("opened "+store.documents.size.toString+" documents in "+since(start))
  val again = now
  Workspace.open(root.tt)
  say("opened again in "+since(again))

  List.range(0, 2).each: round =>
    val begin = now
    val count = store.descents.size
    say("descents: "+count.toString+" faults in "+since(begin))

  revisions.each: revision =>
    List.range(0, 2).each: round =>
      val begin = now
      val name = Sourcing.admit(store, revision.tt)(1)
      say("admitted "+revision+" as "+name.s+" in "+since(begin))

// Times the content of every document at each value of an axis, the other axes at the values
// given: with the store read afresh for each value, as a command for each reads it, and then
// with one store for them all:
// `java -cp <test jar> orthant.timingChain <store's home> <axis> <value>…`.
@main
def timingChain(root: String, axis: String, values: String*): Unit =
  import strategies.throwUnsafely
  import threads.virtualThreads
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)

  def along(store: Workspace): List[Point] =
    val from = store.locate(store.space.root, List.from(values.map(_.tt)))
    val found = store.space.axes.filter(_.name == axis.tt).head.or(panic(m"no such axis"))
    found.values.map: value => from.move(found, value.name)

  def read(store: Workspace, points: List[Point]): Int =
    supervise(store.prepare(points))

    points.fold(0): (count, point) =>
      count + store.documents.filter { document => store.text(document, point).present }.size

  val first = Workspace.open(root.tt)
  val count = along(first).size
  val start = now

  val apart = List.range(0, count).fold(0): (sum, index) =>
    val store = Workspace.open(root.tt)
    sum + read(store, along(store).excerpt(index, index + 1))

  say(count.toString+" values, a store for each: "+apart.toString+" documents in "+since(start))
  val again = now
  val store = Workspace.open(root.tt)
  val together = read(store, along(store))
  say(count.toString+" values, one store: "+together.toString+" documents in "+since(again))

// Times a check-in of one changed line in the longest document at a point, with nothing
// written to the store: the point is checked out, so this is for a copy of a store:
// `java -cp <test jar> orthant.timingTree <store's home> <rounds> <value>…`.
@main
def timingTree(root: String, rounds: Int, values: String*): Unit =
  import strategies.throwUnsafely
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)
  val opened = Workspace.open(root.tt)
  val (store, point) = Sourcing.locate(opened, opened.space.root, List.from(values.map(_.tt)))
  val begin = now
  val count = Worktree.checkout(store, point, true, Worktree.Mode.Plain)
  say("checked out "+count.toString+" documents in "+since(begin))

  val document: Text =
    store.documents.map: document =>
      (store.dossier(document).content(point).let(_.size).or(0), document)

    . stdlib.sortBy(-_(0)).head(1)

  val file = t"${store.tree}/$document"
  val lines = Store.lines(Disk.read(file).or(t""))
  val edited = lines.stdlib.updated(lines.size/2, t"a changed line").pipe(List.from(_))
  Disk.write(file, Store.text(edited), store.staging)
  say(document.s+" has "+lines.size.toString+" lines; "+store.points.size.toString+" points")

  List.range(0, rounds).each: round =>
    val fresh = Sourcing.locate(Workspace.open(root.tt), opened.space.root, Nil)(0)
    val start = now
    val tally = Worktree.checkin(fresh, List(document), false, true)
    say("check-in, a store newly read: "+tally.effects.size.toString+" effects in "+since(start))
    val again = now
    Worktree.checkin(fresh, List(document), false, true)
    say("check-in, the same store again: "+since(again))

// Times what a daemon does for a call which reads one document at a point, several times in
// turn and then that many at once: the store is opened as a command opens it, kept from one
// call to the next: `java -cp <test jar> orthant.timingKept <store's home> <rounds> <value>…`.
@main
def timingKept(root: String, rounds: Int, values: String*): Unit =
  import strategies.throwUnsafely
  import threads.virtualThreads
  import probates.cancelProbate
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000L).toString+"µs"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)

  def call(): Int =
    val opened = Sourcing.viewing(Kept.open(root.tt))
    val (store, point) = Sourcing.locate(opened, opened.space.root, List.from(values.map(_.tt)))
    store.documents.head.let(store.content(_, point)).let(_.size).or(0)

  List.range(0, rounds).each: round =>
    val begin = now
    val lines = call()
    say("a call: "+lines.toString+" lines in "+since(begin))

  val begin = now
  val tasks = supervise(List.range(0, rounds).map { round => async(call()) }.map(_.await()))
  say(rounds.toString+" calls at once: "+tasks.size.toString+" in "+since(begin))
