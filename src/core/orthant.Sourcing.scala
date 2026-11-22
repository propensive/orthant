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

  // The repository at a path. Each tree is read once, however many documents are beneath it,
  // and one thing is read at a time.
  def upstream(directory: Text): Repository = Repository(directory)

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

    entry.value.filter(_.commit == hash).head.lay(insert): value => (store, value.name)

  // The point which `from` becomes when moved to each value a term names, in a store which has
  // those values: on a sourced axis a term may be any revision of the source, and the commit it
  // names is made a value if it is not one.
  def locate(store: Workspace, from: Point, terms: List[Text])
  :   (Workspace, Point) raises Store.Error raises Workspace.Error =

    terms.fold((store, from)): (state, term) =>
      val (store, point) = state

      val revision: Optional[Text] = sourced(store).let: entry =>
        term.cut(t"=") match
          case axis :: revision :: Nil =>
            if axis == entry.name && store.named(axis, revision).absent then revision else Unset

          case _ =>
            if store.space.axes.exists { axis => store.named(axis.name, term).present } then Unset
            else term

      revision.lay((store, store.locate(point, List(term)))): revision =>
        val (next, name) = admit(store, revision)
        (next, next.locate(point, List(t"${sourced(store).let(_.name).or(t"")}=$name")))

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

  // A repository on disk.
  class Repository(directory: Text) extends Upstream:
    private val roots = scala.collection.mutable.HashMap[Text, Text]()
    private val trees = scala.collection.mutable.HashMap[Text, List[(Text, Text, Perusable.Kind)]]()

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
      reading:
        trees.getOrElseUpdate(hash, locally:
          directory.as[Path on Local].open[Git.Repo](): repository ?=>
            repository.treeAt(Git.Hash.unsafe(hash)).map: entry =>
              import Git.Object.Tree.Mode

              val kind = entry.mode match
                case Mode.Directory                 => Perusable.Kind.Directory
                case Mode.Regular | Mode.Executable => Perusable.Kind.File
                case _                              => Perusable.Kind.Other

              (entry.name, entry.hash.text, kind))

    private def root(commit: Text): Text raises Store.Error =
      reading:
        roots.getOrElseUpdate(commit, locally:
          directory.as[Path on Local].open[Git.Repo](): repository ?=>
            repository.commitAt(Git.Hash.unsafe(commit)).tree.text)

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

    def ancestor(upper: Text, lower: Text): Boolean raises Store.Error =
      upper == lower || reading:
        directory.as[Path on Local].open[Git.Repo](): repository ?=>
          repository.isAncestor(Git.Hash.unsafe(upper), Git.Hash.unsafe(lower))

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
