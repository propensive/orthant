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

    val issues = store.documents.map { document => store.dossier(document).issues(point).size }
    val pending = store.documents.map { document => store.dossier(document).pending(point).size }

    say:
      store.name(point).s+": "+lines.fold(0)(_ + _).toString+" lines, "+issues.fold(0)(_ + _).toString+
        " issues, "+pending.fold(0)(_ + _).toString+" pending in "+since(begin)

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
    store.documents.map { document => (store.dossier(document).content(last).let(_.size).or(0), document) }
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

        say:
          document.s+" at "+store.name(point).s+" ("+lines.size.toString+" lines): unchanged "+first+
            " (same records: "+unchanged.toString+"), one line changed "+since(again)+" (ok: "+ok.toString+")"

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

    say:
      store.name(point).s+": exported "+count.toString+" in "+exported+"; "+sum.pending.toString+
        " pending, "+sum.stale.toString+" stale, "+sum.faults.toString+" faults in "+since(again)

  say("total "+since(start))

// Prints a document of a store on disk at the point whose directory is named, `root` for the
// root: `java -cp <test jar> orthant.catDocument <store's home> <document> <point>`.
@main
def catDocument(root: String, document: String, name: String): Unit =
  import strategies.throwUnsafely
  val store = Store.inspect(Disk.path(root.tt))
  val point = store.points.filter { point => store.name(point) == name.tt }.head.or(panic(m"no such point"))
  val lines = store.dossier(document.tt).content(point).or(Nil)
  java.lang.System.out.nn.print(Store.text(lines).s)

// Checks documents in at the point whose directory is named, each from a file of its content:
// `java -cp <test jar> orthant.checkinFiles <store's home> <point> <directory> <document>…`,
// where the content of each document is the file of its name in the directory.
@main
def checkinFiles(root: String, name: String, directory: String, documents: String*): Unit =
  import strategies.throwUnsafely
  val loaded = Workspace.load(root.tt)
  val point = loaded.space.points.filter { point => loaded.name(point) == name.tt }.head.or(panic(m"no such point"))

  documents.foldLeft(loaded): (store, document) =>
    val text = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(directory, document)), "UTF-8")
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
  val point = store.space.points.filter { point => store.name(point) == name.tt }.head.or(panic(m"no such point"))

  store.documents.each: document =>
    val work = Survey.work(store, document, point)
    if work.wanting then java.lang.System.out.nn.println:
      document.s+"\t"+work.pending.size.toString+"\t"+work.stale.size.toString+"\t"+work.faults.toString

// Times reading a repository's references and asking, of every pair of tags, whether one is an
// ancestor of the other: `java -cp <test jar> orthant.timingSync <repository>`.
@main
def timingSync(root: String): Unit =
  import strategies.throwUnsafely
  val start = java.lang.System.nanoTime

  Sync.reading(root.tt): refs =>
    val read = java.lang.System.nanoTime
    val hashes = refs.tags.map(_(1))
    var count = 0
    hashes.each { upper => hashes.each { lower => if refs.ancestor(upper, lower) then count += 1 } }
    val asked = java.lang.System.nanoTime

    java.lang.System.out.nn.println
      ( s"${refs.tags.size} tags, ${refs.branches.size} branches; read in "+
        s"${(read - start)/1000000} ms; ${hashes.size*hashes.size} questions, $count yes, in "+
        s"${(asked - read)/1000000} ms" )
