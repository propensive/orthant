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

import proscenium.{List, Nil, `::`}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.gitBackends.soundnessGit
import soundness.pathInterfaces.pathOnLocal
import soundness.systems.javaBaseSystem

// A git repository as the source of a sourced axis: what is read of it for the content at the
// axis's values, and for placing a commit among them.
object Sourcing:
  // The repository which a directory is in.
  def above(directory: Text): Repository raises Store.Error =
    val missing = Store.Error(Store.Reason.Source(directory, t"there is none"))

    def found(current: Text): Text =
      if Disk.exists(t"$current/.git") then current
      else if current == t"" then abort(missing)
      else found(Disk.parent(current))

    upstream(found(directory))

  // The repositories which have been read, each by its path: a commit, and a tree, is the same
  // however long it is kept, so what is read of one is kept for as long as the process runs.
  private val repositories = scala.collection.concurrent.TrieMap[Text, Repository]()

  // The repository at a path. Each tree is read once, however many documents are beneath it,
  // and one thing is read at a time.
  def upstream(directory: Text): Repository =
    repositories.getOrElseUpdate(directory, Repository(directory))

  // The repository which a store's source names: at a path beneath the store's directory, or
  // else the repository which that directory is in.
  def repository(root: Text, source: SourceEntry): Repository raises Store.Error =
    // The path is made plain, a `..` taking away the directory before it.
    def plain(path: Text): Text =
      val parts = path.cut(t"/").filter: part => part != t"" && part != t"."

      val kept = parts.fold(List[Text]()): (kept, part) =>
        if part == t".." then kept.excerpt(1, kept.size) else part :: kept

      t"/${kept.reverse.join(t"/")}"

    source.repository.lay(above(root)): path =>
      upstream(plain(if path.starts(t"/") then path else t"$root/$path"))

  private def short(hash: Text): Text = hash.keep(12)

  private def sourced(store: Workspace): Optional[AxisEntry] =
    store.settings.axis.filter(_.source.present).head

  private def git(store: Workspace): Repository raises Store.Error =
    val missing = Store.Error(Store.Reason.Source(store.root, t"it has none"))

    store.upstream match
      case repository: Repository => repository
      case _                      => abort(missing)

  // The first value of an axis which is to have a source: the commit which a revision names,
  // with the tag which names it, if one does.
  def root(root: Text, source: SourceEntry, revision: Text)
  :   ValueEntry raises Store.Error raises Workspace.Error =

    val found = repository(root, source)
    val unknown = Workspace.Error(Workspace.Reason.Revision(revision))
    val hash = found.resolve(revision).or(abort(unknown))
    val tagged = found.resolve(t"refs/tags/$revision") == hash && Store.valid(revision)
    ValueEntry(short(hash), label = if tagged then revision else Unset, commit = hash)

  // The store in which the commit a revision names is a value of the sourced axis, and that
  // value's name. A commit which is not one already is put beneath the nearest of the values
  // which are its ancestors, and above those of that value's children which descend from it;
  // nothing is stored at it, so no content changes, and the configuration is not written.
  def admit(store: Workspace, revision: Text)
  :   (Workspace, Text) raises Store.Error raises Workspace.Error =

    val entry = sourced(store).or(abort(Store.Error(Store.Reason.Unknown(revision))))
    val found = git(store)
    val unknown = Workspace.Error(Workspace.Reason.Revision(revision))
    val hash = found.resolve(revision).or(abort(unknown))

    def insert: (Workspace, Text) =
      val above = entry.value.filter: value => value.commit.lay(false)(found.ancestor(_, hash))

      val nearest = above.filter: value =>
        !above.exists: other =>
          other.name != value.name && found.ancestor(value.commit.or(t""), other.commit.or(t""))

      nearest match
        case parent :: Nil =>
          val name = store.hashed(hash)

          val values = entry.value.map: value =>
            val beneath =
              value.parent == parent.name && value.commit.lay(false)(found.ancestor(hash, _))

            if beneath then value.copy(parent = name) else value

          val value = ValueEntry(name, parent.name, commit = hash)
          val added = entry.copy(value = values + List(value))

          val axes = store.settings.axis.map: other =>
            if other.name == entry.name then added else other

          (store.adopt(store.settings.copy(axis = axes)), name)

        case Nil =>
          if found.shallow
          then abort(Workspace.Error(Workspace.Reason.Shallow(revision, entry.name)))
          else abort(Workspace.Error(Workspace.Reason.Unrooted(revision, entry.name)))

        case _ =>
          abort(Workspace.Error(Workspace.Reason.Lineage(revision, entry.name)))

    entry.value.filter(_.commit == hash).head.lay(store.admitted.getOrElseUpdate(hash, insert)):
      value => (store, value.name)

  // The point which `from` becomes when moved to each value a term names, in a store which has
  // those values: on a sourced axis a term may be any revision of the source, and the commit it
  // names is made a value if it is not one.
  def locate(store: Workspace, from: Point, terms: List[Text])
  :   (Workspace, Point) raises Store.Error raises Workspace.Error =

    val (next, named) = admitting(store, terms)
    (next, next.locate(from, named))

  // The store in which each term which is a revision of the source, and not a value, names a
  // value, with the terms as they name the values of that store: such a term is replaced by
  // the value which the commit is made, on the sourced axis.
  def admitting(store: Workspace, terms: List[Text])
  :   (Workspace, List[Text]) raises Store.Error raises Workspace.Error =

    terms.fold((store, List[Text]())): (state, term) =>
      val (store, named) = state

      val revision: Optional[Text] = sourced(store).let: entry =>
        term.cut(t"=") match
          case axis :: revision :: Nil =>
            if axis == entry.name && store.named(axis, revision).absent then revision else Unset

          case _ =>
            if store.space.axes.exists { axis => store.named(axis.name, term).present } then Unset
            else term

      revision.lay((store, named + List(term))): revision =>
        val (next, name) = admit(store, revision)
        (next, named + List(t"${sourced(store).let(_.name).or(t"")}=$name"))

  // The store in which the value checked out on the sourced axis is a value, as it is not if
  // the commit checked out is one which the configuration does not have.
  def viewing(store: Workspace): Workspace raises Store.Error raises Workspace.Error =
    sourced(store).lay(store): entry =>
      Worktree.checked(store, entry.name).lay(store): value =>
        if store.named(entry.name, value).present then store else admit(store, value)(0)

  // Writes the configuration if it has a value which the store's own does not: a commit at
  // which something has been checked in is one of the axis's values from then on.
  def declare(store: Workspace): Workspace raises Store.Error =
    if sourced(store).absent || Workspace.settings(store.root) == store.settings then store
    else store.inscribe()

  // A repository on disk. What is read of it is kept: the entries of each tree, the tree of
  // each commit, and the parents and the generation of each commit of the history which an
  // ancestry question reads.
  class Repository(directory: Text) extends Upstream:
    private val roots = scala.collection.concurrent.TrieMap[Text, Text]()

    private val trees =
      scala.collection.concurrent.TrieMap[Text, List[(Text, Text, Perusable.Kind)]]()

    // The parents of each commit which has been read, and its generation: one more than the
    // greatest of its parents', so that no commit is an ancestor of one whose generation is
    // not greater than its own.
    private val parents = scala.collection.concurrent.TrieMap[Text, List[Text]]()
    private val generations = scala.collection.concurrent.TrieMap[Text, Int]()

    // The commits at which a shallow history ends, as they were when the parents were read:
    // the parents of such a commit are not in the repository, and it is taken to have none.
    private var boundary: Optional[Text] = Unset

    def name: Text = directory

    private def reading[result]
      ( block: (Tactic[Git.Error], Tactic[Io.Error], Tactic[Path.Error]) ?=> result )
    :   result raises Store.Error =

      def failed(error: fulminate.Error): Store.Error =
        Store.Error(Store.Reason.Source(directory, Orthant.prose(error)))

      mitigate:
        case error: Git.Error  => failed(error)
        case error: Io.Error   => failed(error)
        case error: Path.Error => failed(error)

      . protect(synchronized(block))

    // The entries of a tree: the name, the hash and the kind of each.
    private def tree(hash: Text): List[(Text, Text, Perusable.Kind)] raises Store.Error =
      trees.get(hash).getOrElse:
        reading:
          val found = directory.as[Path on Local].open[Git.Repo](): repository ?=>
            repository.treeAt(Git.Hash.unsafe(hash)).map: entry =>
              import Git.Object.Tree.Mode

              val kind = entry.mode match
                case Mode.Directory                 => Perusable.Kind.Directory
                case Mode.Regular | Mode.Executable => Perusable.Kind.File
                case _                              => Perusable.Kind.Other

              (entry.name, entry.hash.text, kind)

          trees(hash) = found
          found

    private def root(commit: Text): Text raises Store.Error =
      roots.get(commit).getOrElse:
        reading:
          val found = directory.as[Path on Local].open[Git.Repo](): repository ?=>
            repository.commitAt(Git.Hash.unsafe(commit)).tree.text

          roots(commit) = found
          found

    // The hash and the kind of what is at a path in a commit's tree.
    private def find(commit: Text, path: Text)
    :   Optional[(Text, Perusable.Kind)] raises Store.Error =

      def descend(hash: Text, kind: Perusable.Kind, todo: List[Text])
      :   Optional[(Text, Perusable.Kind)] =

        todo match
          case name :: rest =>
            if kind != Perusable.Kind.Directory then Unset
            else tree(hash).filter(_(0) == name).head.let: entry =>
              descend(entry(1), entry(2), rest)

          case _ =>
            (hash, kind)

      descend(root(commit), Perusable.Kind.Directory, path.cut(t"/").filter(_ != t""))

    def blob(commit: Text, path: Text): Optional[Data] raises Store.Error =
      find(commit, path).let: (hash, kind) =>
        if kind != Perusable.Kind.File then Unset else reading:
          directory.as[Path on Local].open[Git.Repo](): repository ?=>
            repository.blobAt(Git.Hash.unsafe(hash))

    // Whether `upper` is `lower` or an ancestor of it. The history beneath a commit is read
    // once, to give each commit in it a generation; a question is then answered from the
    // commits between the two, since none whose generation is not greater than `upper`'s
    // descends from it.
    def ancestor(upper: Text, lower: Text): Boolean raises Store.Error =
      upper == lower || reading:
        val ends = Disk.read(t"$directory/.git/shallow").or(Disk.read(t"$directory/shallow"))

        if ends != boundary then
          parents.clear()
          generations.clear()
          boundary = ends

        val shallow: scala.collection.immutable.Set[Text] =
          ends.lay(Nil)(Store.lines(_)).stdlib.toSet

        directory.as[Path on Local].open[Git.Repo](): repository ?=>
          def above(commit: Text): List[Text] = parents.get(commit).getOrElse:
            val found =
              if shallow(commit) then Nil
              else repository.commitAt(Git.Hash.unsafe(commit)).parents.map(_.text)

            parents(commit) = found
            found

          def generation(commit: Text): Int = generations.getOrElse(commit, 0)

          // Each commit is given its generation after its parents have theirs.
          @scala.annotation.tailrec
          def number(todo: List[Text]): Unit = todo match
            case commit :: rest =>
              if generations.contains(commit) then number(rest) else
                val known = above(commit)
                val wanting = known.filter(!generations.contains(_))

                if wanting.nil then
                  val most = known.fold(0): (most, parent) => most.max(generation(parent))
                  generations(commit) = most + 1
                  number(rest)
                else
                  number(wanting + todo)

            case _ =>
              ()

          // A commit which the repository does not have, as a shallow one may not, is an
          // ancestor of none which it has.
          def held: Boolean =
            generations.contains(upper) || repository.exists(Git.Hash.unsafe(upper))

          if held then number(List(upper, lower))
          val floor = generation(upper)
          val seen = scala.collection.mutable.HashSet[Text]()

          @scala.annotation.tailrec
          def descends(todo: List[Text]): Boolean = todo match
            case commit :: rest =>
              if commit == upper then true
              else if generation(commit) <= floor || !seen.add(commit) then descends(rest)
              else descends(above(commit) + rest)

            case _ =>
              false

          held && descends(List(lower))

    def entries(commit: Text, directory: Text): List[Perusable.Entry] raises Store.Error =
      find(commit, directory).lay(Nil): (hash, kind) =>
        if kind != Perusable.Kind.Directory then Nil
        else tree(hash).map: (name, hash, kind) => Perusable.Entry(name, kind)

    // The hash of the commit which a revision names, if it names one.
    def resolve(revision: Text): Optional[Text] raises Store.Error =
      reading:
        directory.as[Path on Local].open[Git.Repo](): repository ?=>
          repository.resolve(Git.Revision.unsafe(t"$revision^{commit}")).value.let(_.text)

    // Whether the repository has only the later part of its history.
    def shallow: Boolean raises Store.Error =
      Disk.exists(t"$directory/.git/shallow") || Disk.exists(t"$directory/shallow")
