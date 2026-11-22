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
import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.filesystemOptions.createParents
import soundness.filesystemOptions.deleteRecursively
import soundness.filesystemOptions.dereferenceSymlinks
import soundness.filesystemOptions.moveAtomically
import soundness.filesystemOptions.overwritePreexisting
import soundness.gitBackends.soundnessGit
import soundness.pathInterfaces.pathOnLocal
import soundness.sortingAlgorithms.timsort
import soundness.systems.javaBaseSystem

// Following a git repository with an axis. A tag which the axis's pattern takes becomes a
// value, named by the start of its commit's hash and labelled with the tag, beneath the nearest
// value whose commit is an ancestor of it. A branch which the axis tracks has a value of its
// own name for what is not yet released on it. When a new tag is found on that branch, the
// branch's value becomes the tag's, with everything stored at it, and a new, empty value takes
// its place beneath: what was checked in since the last release is what was released.
object Sync:
  // What a sync needs of a repository: its tags and branches, each with its commit's hash,
  // the branch which is checked out, and whether one commit is an ancestor of another.
  case class Refs
    ( tags:     List[(Text, Text)],
      branches: List[(Text, Text)],
      head:     Optional[Text],
      ancestor: (Text, Text) => Boolean )

  // Whether a name is one the pattern takes: `*` stands for any run of characters.
  def takes(pattern: Text, name: Text): Boolean =
    def recur(parts: List[Text], rest: Text, first: Boolean): Boolean = parts match
      case part :: Nil =>
        if first then rest == part else rest.ends(part)

      case part :: tail =>
        if first then rest.starts(part) && recur(tail, rest.skip(part.length), false)
        else
          List.range(0, rest.length - part.length + 1).exists: index =>
            rest.skip(index).starts(part) && recur(tail, rest.skip(index + part.length), false)

      case _ =>
        true

    recur(pattern.cut(t"*"), name, true)

  // Runs a block with the references of the repository which the directory is in.
  def reading[result](directory: Text)(block: Refs => result)
  :   result raises Store.Error raises Workspace.Error =

    val missing = Workspace.Error(Workspace.Reason.Repository(t"there is none above $directory"))

    def found(current: Text): Text =
      if Disk.exists(t"$current/.git") then current
      else if current == t"" then abort(missing)
      else found(Disk.parent(current))

    val root = found(directory)

    mitigate:
      case error: Git.Error  => Workspace.Error(Workspace.Reason.Repository(Orthant.prose(error)))
      case error: Io.Error   => Workspace.Error(Workspace.Reason.Repository(Orthant.prose(error)))
      case error: Path.Error => Workspace.Error(Workspace.Reason.Repository(Orthant.prose(error)))

    . protect:
        root.as[Path on Local].open[Git.Worktree](): worktree ?=>
          val head = worktree.head().value match
            case Git.Head.Attached(branch, _) => branch.text
            case _                            => Unset

          val tags = worktree.tags().value.to[List].map: (tag, hash) => (tag.text, hash.text)

          val branches = worktree.branches().value.to[List].map: (branch, hash) =>
            (branch.text, hash.text)

          // The history beneath every tag and branch is read once: each commit's parents. The
          // branches' histories are read whole; what a tag adds is read a commit at a time.
          val parents = scala.collection.mutable.HashMap[Text, scala.List[Text]]()

          branches.each: (branch, tip) =>
            if !parents.contains(tip) then
              worktree.log(Git.Revision.unsafe(tip)).value.each: commit =>
                parents(commit.commit.text) = commit.parents.stdlib.toList.map(_.text)

          tags.each: (tag, hash) =>
            val todo = scala.collection.mutable.Stack[Text](hash)

            @tailrec
            def explore(): Unit = if !todo.isEmpty then
              val next = todo.pop()

              if !parents.contains(next) then
                val above: scala.List[Text] = worktree.show(Git.Hash.unsafe(next)) match
                  case Git.Object.Commit(commit) => commit.parents.stdlib.toList.map(_.text)
                  case _                         => scala.Nil

                parents(next) = above
                above.foreach: parent => if !parents.contains(parent) then todo.push(parent)

              explore()

            explore()

          // Which of the tagged commits and branch tips each commit has beneath it, itself
          // included: a set of bits, made for each commit from its parents', parents first.
          val marked: scala.Array[Text] =
            (tags.stdlib.map(_(1)) ++ branches.stdlib.map(_(1))).distinct.toArray

          val bit = scala.collection.mutable.HashMap[Text, Int]()
          marked.indices.foreach: index => bit(marked(index)) = index
          val words = (marked.length + 63)/64
          val beneath = scala.collection.mutable.HashMap[Text, scala.Array[Long]]()

          parents.keys.foreach: start =>
            val todo = scala.collection.mutable.Stack[(Text, Boolean)]((start, false))

            // A commit's bits are made once its parents' are, or once they are being made.
            def ready(parent: Text): Boolean = beneath.contains(parent) || !parents.contains(parent)

            @tailrec
            def explore(): Unit = if !todo.isEmpty then
              val (commit, expanded) = todo.pop()

              if !beneath.contains(commit) then
                val above = parents.getOrElse(commit, scala.Nil)

                if expanded || above.forall(ready) then
                  val bits = new scala.Array[Long](words)

                  above.foreach: parent =>
                    beneath.get(parent).foreach: theirs =>
                      bits.indices.foreach: word => bits(word) |= theirs(word)

                  bit.get(commit).foreach: index => bits(index/64) |= 1L << (index%64)
                  beneath(commit) = bits
                else
                  todo.push((commit, true))

                  above.foreach: parent =>
                    if !beneath.contains(parent) && parents.contains(parent)
                    then todo.push((parent, false))

              explore()

            explore()

          val known = scala.collection.mutable.HashMap[(Text, Text), Boolean]()

          def ancestor(upper: Text, lower: Text): Boolean =
            if upper == lower then true else
              (bit.get(upper), beneath.get(lower)) match
                case (Some(index), Some(bits)) => (bits(index/64) & (1L << (index%64))) != 0

                case _ =>
                  known.getOrElseUpdate
                    ( (upper, lower),
                      worktree.isAncestor(Git.Hash.unsafe(upper), Git.Hash.unsafe(lower)) )

          block(Refs(tags, branches, head, ancestor))

  private def short(hash: Text): Text = hash.keep(12)

  // The nearest of the axis's commits which are ancestors of a commit, or the commit itself if
  // `inclusive` is set: the root's name if there is none.
  private def nearest
    ( store: Workspace, entry: AxisEntry, hash: Text, refs: Refs, inclusive: Boolean, name: Text )
  :   Text raises Store.Error raises Workspace.Error raises Axis.Error =

    val above = entry.value.filter: value =>
      value.commit.let: commit =>
        (inclusive || commit != hash) && (commit == hash || refs.ancestor(commit, hash))

      . or(false)

    val nearest = above.filter: value =>
      !above.exists: other =>
        other.name != value.name &&
          refs.ancestor(value.commit.or(t""), other.commit.or(t"")) && other.commit != value.commit

    nearest match
      case value :: Nil => value.name
      case Nil          => entry.value.filter(_.parent.absent).head.let(_.name).or(t"")
      case _            => abort(Workspace.Error(Workspace.Reason.Lineage(name, entry.name)))

  // The store in which a value of an axis has another name: its records are moved, and the
  // values beneath it follow. The configuration is written if any record is moved, so that it
  // and the records agree; otherwise it is left for the caller to write.
  private def rename
    ( store: Workspace, axis: Text, old: Text, name: Text, change: ValueEntry => ValueEntry )
  :   Workspace raises Store.Error raises Workspace.Error raises Axis.Error =

    def written(value: Text): Text = t"$axis=${value.cut(t"/").join(t"_")}"

    Disk.directories(store.data).each: directory =>
      val pairs = directory.cut(t",")

      if old != name && pairs.has(written(old)) then
        val moved =
          pairs.map { pair => if pair == written(old) then written(name) else pair }.join(t",")

        def failed(error: fulminate.Error): Store.Error =
          Store.Error(Store.Reason.Access(directory, Orthant.prose(error)))

        mitigate:
          case error: Io.Error   => failed(error)
          case error: Path.Error => failed(error)

        . protect:
            val target = t"${store.data}/$moved".as[Path on Local]
            t"${store.data}/$directory".as[Path on Local].moveTo(target)

    val entries = store.settings.axis.map: entry =>
      if entry.name != axis then entry else
        entry.copy(value = entry.value.map: value =>
          if value.name == old then change(value.copy(name = name))
          else if value.parent == old then value.copy(parent = name)
          else value)

    def place(point: Point): Point =
      if point.coordinates.at(axis) == old then Point(point.coordinates.define(axis, name))
      else point

    val renamed = store.adopt(store.settings.copy(axis = entries), place)
    if old != name then renamed.inscribe() else renamed

  private def add(store: Workspace, axis: Text, value: ValueEntry)
  :   Workspace raises Store.Error raises Workspace.Error raises Axis.Error =

    val entries = store.settings.axis.map: entry =>
      if entry.name == axis then entry.copy(value = entry.value + List(value)) else entry

    store.adopt(store.settings.copy(axis = entries))

  private def git(store: Workspace, axis: Text)
  :   AxisEntry raises Store.Error raises Workspace.Error =

    val entry = store.entry(axis).or(abort(Store.Error(Store.Reason.Unknown(axis))))
    if entry.kind != t"git" then abort(Workspace.Error(Workspace.Reason.Kind(axis)))
    entry

  // The store after an axis has taken up the repository's new tags, and the branches named,
  // with a line for each thing done. With no branch named and none tracked, the branch which is
  // checked out is tracked.
  def apply(store: Workspace, axis: Text, refs: Refs, track: List[Text])
  :   (Workspace, List[Text]) raises Store.Error raises Workspace.Error raises Axis.Error =

    val entry = git(store, axis)
    val pattern = entry.pattern.or(t"*")
    // A tag of a commit older than the root's, where the root has one, is not followed.
    val base = entry.value.filter(_.parent.absent).head.let(_.commit)

    val tags =
      refs.tags.filter: (tag, hash) => takes(pattern, tag) && base.lay(true)(refs.ancestor(_, hash))
      . order(_(0))

    // A tag of a commit which is already a value is one more label for it.
    val labelled = entry.value.fold((store, List[Text]())): (state, value) =>
      val extra =
        tags.filter { (tag, hash) => value.commit == hash && !value.labels.has(tag) }.map(_(0))

      if extra.nil then state else
        val label = (value.labels + extra).join(t" ")
        val renamed = rename(state(0), axis, value.name, value.name, _.copy(label = label))
        (renamed, state(1) + extra.map { tag => t"$tag is another name for ${value.title}" })

    val commits = tags.map(_(1)).distinct.filter: hash => !entry.value.exists(_.commit == hash)

    // An ancestor before its descendants.
    def before(hash: Text): Int =
      commits.count: other => other != hash && refs.ancestor(other, hash)

    val ordered = commits.order(before(_))

    val tagged = ordered.fold(labelled): (state, hash) =>
      val (store, notes) = state
      val entry = git(store, axis)
      val label = tags.filter(_(1) == hash).map(_(0)).join(t" ")
      val title = label.cut(t" ").head
      val parent = nearest(store, entry, hash, refs, false, title)

      // A branch whose unreleased value lies between that parent and this commit is released.
      val released = entry.value.filter: value =>
        value.branch.or(false) && value.parent == parent && refs.branches.exists: (branch, tip) =>
          branch == value.name && (tip == hash || refs.ancestor(hash, tip))

      def added: (Workspace, List[Text]) =
        val entry = ValueEntry(short(hash), parent, label = label, commit = hash)
        val note = t"added $title beneath ${store.title(axis, parent)}"
        (add(store, axis, entry), notes + List(note))

      released.head.lay(added): branch =>
        val held = store.dossiers.values.to[List].exists: dossier =>
          dossier.folios.keys.to[List].exists: point => point.coordinates.at(axis) == branch.name

        val renamed = rename(store, axis, branch.name, short(hash),
            _.copy(label = label, commit = hash, branch = Unset))

        val fresh = add(renamed, axis, ValueEntry(branch.name, short(hash), branch = true))
        val note = t"released ${branch.name} as $title; ${branch.name} now follows $title"

        val caution = t"warning: everything checked in at ${branch.name} is now part of $title"
        val warning = if held then List(caution) else Nil

        (fresh, notes + List(note) + warning)

    val tracked = git(tagged(0), axis).value.filter(_.branch.or(false)).map(_.name)
    val wanted = if track.nil && tracked.nil then refs.head.lay(Nil)(List(_)) else track

    val synced = wanted.filter(!tracked.has(_)).fold(tagged): (state, branch) =>
      val (store, notes) = state
      if !Store.valid(branch) then abort(Workspace.Error(Workspace.Reason.Name(branch)))

      def unknown: Nothing = abort(Workspace.Error(Workspace.Reason.Branch(branch)))
      val tip = refs.branches.filter(_(0) == branch).head.let(_(1)).or(unknown)

      val parent = nearest(store, git(store, axis), tip, refs, true, branch)

      ( add(store, axis, ValueEntry(branch, parent, branch = true)),
        notes + List(t"tracking $branch beneath ${store.title(axis, parent)}") )

    // The configuration is written once, with everything which was done.
    if synced(0).settings != store.settings then synced(0).inscribe()
    synced

  // The store in which a tag is a value beneath a parent which is given, for a tag whose
  // nearest ancestor a sync could not choose.
  def pin(store: Workspace, axis: Text, tag: Text, parent: Text, refs: Refs)
  :   Workspace raises Store.Error raises Workspace.Error raises Axis.Error =

    val entry = git(store, axis)

    val hash =
      refs.tags.filter(_(0) == tag).head.let(_(1)).or(abort(Store.Error(Store.Reason.Unknown(tag))))

    val above = store.named(axis, parent).or(abort(Store.Error(Store.Reason.Unknown(parent))))

    if entry.value.exists(_.commit == hash)
    then abort(Workspace.Error(Workspace.Reason.ValueExists(axis, tag)))

    add(store, axis, ValueEntry(short(hash), above, label = tag, commit = hash)).inscribe()
