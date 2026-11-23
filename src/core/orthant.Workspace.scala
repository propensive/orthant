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

import proscenium.{List, Nil, Map}
import soundness.*
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.probates.cancelProbate
import soundness.systems.javaBaseSystem

// A store as the tool has it: on a disk, beneath a directory, where it is changed as well as
// read. It is read by the reader (`Store`, which it is), and what is here besides is the
// writing of it, and the errors of that.
object Workspace:
  enum Reason:
    case Exists(directory: Text)
    case Name(name: Text)
    case AxisExists(axis: Text)
    case ValueExists(axis: Text, value: Text)
    case Edited(count: Int)
    case Moved(document: Text)
    case Suffix(document: Text)
    case Refused(document: Text, point: Text, detail: Text, code: fulminate.Bolt)
    case Rootless(point: Text)
    case Preference(axis: Text)
    case Against(axis: Text, value: Text)
    case Repository(detail: Text)
    case Lineage(tag: Text, axis: Text)
    case Branch(name: Text)
    case Kind(axis: Text)
    case Clash(document: Text)
    case Unmoved(value: Text)
    case Revision(revision: Text)
    case Unrooted(revision: Text, axis: Text)
    case Shallow(revision: Text, axis: Text)
    case Sourced(axis: Text)
    case Lifeless
    case Vacant(directory: Text)
    case Live(point: Text, directory: Text)
    case Unworking
    case Tagged(axis: Text)

  case class Error(reason: Reason)(using Diagnostics)
  extends fulminate.Error
    ( reason match
        case Reason.Exists(directory) =>
          m"there is already an Orthant store in $directory"

        case Reason.Name(name) =>
          m"$name is not a valid name for an axis or a value"

        case Reason.AxisExists(axis) =>
          m"the axis $axis already exists"

        case Reason.ValueExists(axis, value) =>
          m"the axis $axis already has the value $value"

        case Reason.Edited(count) =>
          m"""
            the working tree has edits which are not checked in, to $count of its documents;
            `orthant diff` shows them, `orthant checkin` stores them, and `orthant checkout --force`
            discards them
          """

        case Reason.Moved(document) =>
          m"the store's content of $document has changed since it was checked out"

        case Reason.Suffix(document) =>
          m"the document $document has a name ending in .delta"

        case Reason.Refused(document, point, detail, _) =>
          m"$document at $point: $detail"

        case Reason.Rootless(point) =>
          m"the point $point has no storage parent to be the reference of an indirect view"

        case Reason.Against(axis, value) =>
          m"""
            there is no view against the change from $value on the axis $axis: the change must be
            from an earlier value than the one checked out, as in `orthant ref <earlier>..<checked
            out>`, and on an axis other than the one the documents are derived along
          """

        case Reason.Unmoved(value) =>
          m"""
            $value is already checked out, so there is no change from it to show; name an earlier
            value, as in `orthant ref <earlier>..$value`
          """

        case Reason.Clash(document) =>
          m"the edits to $document cannot be carried onto what the store now has for it"

        case Reason.Repository(detail) =>
          m"the git repository could not be read: $detail"

        case Reason.Lineage(tag, axis) =>
          m"""
            $tag has more than one nearest ancestor among the values of the axis $axis; give its
            parent with `orthant axis value $axis $tag <parent>`
          """

        case Reason.Branch(name) =>
          m"the repository has no branch $name"

        case Reason.Kind(axis) =>
          m"the axis $axis does not follow a git repository"

        case Reason.Revision(revision) =>
          m"the source's repository has no commit $revision"

        case Reason.Unrooted(revision, axis) =>
          m"the commit $revision does not descend from the root of the axis $axis"

        case Reason.Shallow(revision, axis) =>
          m"""
            the history of the source's repository is shallow, and does not reach from $revision
            to a value of the axis $axis
          """

        case Reason.Sourced(axis) =>
          m"the axis $axis takes its values from the commits of its source, and has no branches"

        case Reason.Lifeless =>
          m"""
            the store has no live tree; `orthant init --live <directory>` makes a store which
            has one
          """

        case Reason.Vacant(directory) =>
          m"the live tree $directory is not there, and nothing is stored from a tree which is not"

        case Reason.Live(point, directory) =>
          m"""
            the documents at $point are those of the live tree; edit them in $directory, and
            store them with `orthant tag`
          """

        case Reason.Unworking =>
          m"""
            no value of the store is the one being worked on, to be released; `orthant axis
            working <axis> <value>` makes a value that one
          """

        case Reason.Tagged(axis) =>
          m"""
            the axis $axis takes its values from the tags of the repository; tag the commit with
            git, and `orthant sync` makes the release
          """

        case Reason.Preference(axis) =>
          m"the axis $axis has hunks stored on it, so what it prefers cannot change" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Name(_)                => Orthant.bolt(2, 4)
      case Reason.Revision(_)            => Orthant.bolt(7, 7)
      case Reason.Unrooted(_, _)         => Orthant.bolt(7, 8)
      case Reason.Shallow(_, _)          => Orthant.bolt(7, 9)
      case Reason.Sourced(_)             => Orthant.bolt(7, 10)
      case Reason.AxisExists(_)          => Orthant.bolt(2, 5)
      case Reason.ValueExists(_, _)      => Orthant.bolt(2, 6)
      case Reason.Preference(_)          => Orthant.bolt(2, 9)
      case Reason.Rootless(_)            => Orthant.bolt(4, 10)
      case Reason.Clash(_)               => Orthant.bolt(4, 12)
      case Reason.Against(_, _)          => Orthant.bolt(4, 13)
      case Reason.Unmoved(_)             => Orthant.bolt(4, 14)
      case Reason.Refused(_, _, _, code) => code
      case Reason.Edited(_)              => Orthant.bolt(4, 2)
      case Reason.Moved(_)               => Orthant.bolt(4, 3)
      case Reason.Suffix(_)              => Orthant.bolt(4, 4)
      case Reason.Repository(_)          => Orthant.bolt(7, 1)
      case Reason.Lineage(_, _)          => Orthant.bolt(7, 2)
      case Reason.Branch(_)              => Orthant.bolt(7, 3)
      case Reason.Kind(_)                => Orthant.bolt(7, 4)
      case Reason.Exists(_)              => Orthant.bolt(5, 2)
      case Reason.Lifeless               => Orthant.bolt(4, 15)
      case Reason.Vacant(_)              => Orthant.bolt(4, 16)
      case Reason.Live(_, _)             => Orthant.bolt(4, 17)
      case Reason.Unworking              => Orthant.bolt(2, 10)
      case Reason.Tagged(_)              => Orthant.bolt(7, 11)

  // How many documents `survey` derives at once.
  private val abreast: Int = 4

  private def folder(root: Text): Text = t"$root/.orthant"
  private def config(root: Text): Text = t"$root/.orthant/config.tel"

  // The directory, at or above the one given, which has a store in it.
  def find(directory: Text): Text raises Store.Error = Store.find(Disk.path(directory)).encode

  // Creates an empty store, with no axes, whose working tree is the directory `tree`. If it
  // is given a live tree, that is the directory `live`, which is not ignored: its documents are
  // committed as they are written. A working tree within the store's own uncommitted directory
  // needs no ignoring either.
  def init(root: Text, tree: Text, live: Optional[Text] = Unset)
  :   Workspace raises Store.Error raises Error =

    if Disk.exists(config(root)) then abort(Error(Reason.Exists(root)))
    val staging = t"${folder(root)}/work/staging"
    val settings = Settings(Settings.format, tree, Nil, live)
    Disk.write(t"${folder(root)}/.gitattributes", t"* -text\n", staging)
    Disk.write(t"${folder(root)}/.gitignore", t"/work/\n", staging)
    val ignore = Disk.read(t"$root/.gitignore").or(t"")

    if !tree.starts(t".orthant/work/") && !Store.lines(ignore).has(t"/$tree/") then
      val ignored = Store.text(Store.lines(ignore) + List(t"/$tree/"))
      Disk.write(t"$root/.gitignore", ignored, staging)

    Workspace(root, new Store(settings, Store.sound(settings.space), Map())).configure(settings)

  // The configuration of the store beneath `root`, alone: no record is read.
  def settings(root: Text): Settings raises Store.Error = Store.configuration(Disk.path(root))

  // The store beneath `root`, and everything which is wrong with what it holds. A file which
  // cannot be read as a record is left out.
  def scan(root: Text): (Workspace, List[Store.Error]) raises Store.Error =
    val store = inspect(root)
    (store, store.faults)

  // The store beneath `root`, with whatever of its records can be read; the rest are among its
  // `omitted`.
  def inspect(root: Text): Workspace raises Store.Error =
    Workspace(root, Store.inspect(Disk.path(root), upstream(root, settings(root))))

  // The repository which a store's sourced axis, if it has one, takes its content from.
  def upstream(root: Text, settings: Settings): Optional[Upstream] raises Store.Error =
    settings.axis.filter(_.source.present).head.let(_.source).let: source =>
      Sourcing.repository(root, source)

  // The store beneath `root`, which must have nothing wrong with it.
  def load(root: Text): Workspace raises Store.Error =
    val (store, faults) = scan(root)
    faults.head.let(abort(_))
    store

  // The store beneath `root`, of which every record must be readable. Nothing is derived, so a
  // hunk which has no one place is not found here: it is found when the document it is in is
  // read at its point, or changed, and `scan` finds them all.
  def open(root: Text): Workspace raises Store.Error =
    val store = inspect(root)
    store.omitted.head.let(abort(_))
    store

// A store on disk: the directory `root` which has `.orthant` in it, and the store as it was
// read from there, or as it has since been made.
case class Workspace(root: Text, held: Store)
extends Store(held.settings, held.space, held.dossiers, held.omitted, held.upstream):
  import Workspace.{Error, Reason}

  val folder: Text = t"$root/.orthant"
  val data: Text = t"$folder/data"
  val work: Text = t"$folder/work"
  val staging: Text = t"$work/staging"
  val lock: Text = t"$work/lock"
  val tree: Text = t"$root/${settings.tree}"

  // The live tree, if the store has one: its directory, and the point whose documents it
  // holds, which is the working value on that value's axis, and the root of every other. In a
  // store with no working value it is the root point.
  def live: Optional[(Text, Point)] = settings.live.let: directory =>
    val point = settings.working.lay(space.root): (entry, value) =>
      space.axes.filter(_.name == entry.name).head.lay(space.root)(space.root.move(_, value.name))

    (t"$root/$directory", point)

  private def file(point: Point, document: Text, folio: Optional[Folio]): Optional[Text] =
    folio.let:
      case Folio.Base(_) => t"$data/${name(point)}/$document"
      case _             => t"$data/${name(point)}/$document.delta"

  // The name of a value which is a commit: the start of the commit's hash, or the whole of it
  // if a value of some axis has that name already.
  def hashed(hash: Text): Text raises Error =
    val short = hash.keep(12)

    if owner(short).absent then short
    else owner(hash).lay(hash): axis => abort(Error(Reason.ValueExists(axis.name, hash)))

  private def holding(dossiers: Map[Text, Dossier]): Workspace =
    Workspace(root, new Store(settings, space, dossiers, omitted, upstream))

  // What this store becomes when a commit is made a value of its sourced axis, and that
  // value's name: found once for each commit, and what is derived in that store is kept with it.
  private[orthant] val admitted: scala.collection.concurrent.TrieMap[Text, (Workspace, Text)] =
    scala.collection.concurrent.TrieMap()

  // This store, with the dossier which another has for each document of which the two hold
  // the same records: what has been derived of the document in the other is not derived again
  // in this one. The two must have one configuration, and one source, for any to be shared.
  def sharing(former: Workspace): Workspace =
    if former.settings != settings || former.upstream != upstream then this else
      val shared = documents.fold(dossiers): (shared, document) =>
        former.dossiers.at(document).lay(shared): other =>
          if other.folios == dossier(document).folios then shared.define(document, other)
          else shared

      Workspace(root, new Store(settings, former.space, shared, omitted, upstream))

  // The point which `from` becomes when moved to each value given, as `axis=value`, or as a
  // value alone where one axis has it.
  def locate(from: Point, values: List[Text]): Point raises Store.Error = point(values, from)

  // The store in which a document has the dossier given; the files of each point whose folio
  // differs are written, or deleted.
  def save(document: Text, updated: Dossier): Workspace raises Store.Error =
    val former = dossier(document)

    points.each: point =>
      val old = former.folios.at(point)
      val folio = updated.folios.at(point)

      if old != folio then
        val target = file(point, document, folio)

        file(point, document, old).let: path => if path != target then Disk.remove(path, data)

        target.let: path =>
          val content: List[Text] = folio match
            case Folio.Base(lines)    => lines
            case delta: Folio.Delta   => Delta.serialize(delta)
            case absent: Folio.Absent => Delta.serialize(absent)
            case _                    => Nil

          Disk.write(path, Store.text(content), staging)

    val present = updated.folios.keys.to[List].nil
    holding(if present then dossiers.omit(document) else dossiers.define(document, updated))

  // The store with a new configuration, which is written.
  def configure(settings: Settings): Workspace raises Store.Error = adopt(settings).inscribe()

  // Writes the configuration the store has.
  def inscribe(): Workspace raises Store.Error =
    Disk.write(t"$folder/config.tel", Store.text(Store.lines(settings.in[Tel].show)), staging)
    this

  // The store with a new configuration, which is not written. A record at a point of this
  // store is, in that one, at the point which `place` gives for it.
  def adopt(settings: Settings, place: Point => Point = point => point)
  :   Workspace raises Store.Error =

    val space = Store.sound(settings.space)
    val origin = if settings.axis.exists(_.source.present) then upstream else Unset

    // Each dossier is of the space it was read in, so it is made again in the new one.
    val moved = documents.fold(Map[Text, Dossier]()): (moved, document) =>
      val former = dossier(document).folios

      val folios = former.keys.to[List].fold(Map[Point, Folio]()): (folios, point) =>
        former.at(point).lay(folios)(folios.define(place(point), _))

      moved.define(document, Dossier(space, folios, Store.source(settings, origin, document)))

    Workspace(root, new Store(settings, space, moved, omitted, origin))

  // The store with a new axis, which has the one value `value`. It is last in precedence,
  // unless the last axis has a source, which it comes before. If it is given a source, `value`
  // is a revision of the source's repository, and its root is the commit which that names.
  def axis
    ( name:    Text,
      value:   Text,
      kind:    Text,
      pattern: Optional[Text]        = Unset,
      source:  Optional[SourceEntry] = Unset )
  :   Workspace raises Store.Error raises Error =

    if !Store.valid(name) || name.contains(t"/") then abort(Error(Reason.Name(name)))
    if settings.axis.exists(_.name == name) then abort(Error(Reason.AxisExists(name)))
    val unsourced = settings.axis.filter(_.source.absent)
    val sourced = settings.axis.filter(_.source.present)

    source.lay:
      if !Store.valid(value) then abort(Error(Reason.Name(value)))
      owner(value).let: other => abort(Error(Reason.ValueExists(other.name, value)))
      val entry = AxisEntry(name, kind, t"own", List(ValueEntry(value)), pattern)
      configure(settings.copy(axis = unsourced + List(entry) + sourced))

    . apply: source =>
        if !sourced.nil then abort(Error(Reason.AxisExists(sourced.head.let(_.name).or(name))))

        // What is stored at the root would be at a point whose content is the source's.
        documents.each: document =>
          if dossier(document).folios.defines(space.root)
          then abort(Store.Error(Store.Reason.Sourced(t"$data/${this.name(space.root)}/$document")))

        val found = Sourcing.root(this.root, source, value)
        val root = found.copy(name = found.commit.lay(found.name)(hashed(_)))
        val entry = AxisEntry(name, kind, t"own", List(root), pattern, source)
        Workspace.open(configure(settings.copy(axis = unsourced + List(entry))).root)

  // The store with a new value on an axis, beneath `parent` or else beneath the axis's root.
  def value(axis: Text, value: Text, parent: Optional[Text])
  :   Workspace raises Store.Error raises Error =

    val unknown = Store.Error(Store.Reason.Unknown(axis))
    val found = space.axes.filter(_.name == axis).head.or(abort(unknown))

    // On a sourced axis a value is a commit, and its parent is whichever value its history
    // gives it.
    if !found.sourced then ordinary(found, value, parent)
    else Sourcing.admit(this, value)(0).inscribe()

  private def ordinary(found: Axis, value: Text, parent: Optional[Text])
  :   Workspace raises Store.Error raises Error =

    val axis = found.name
    if !Store.valid(value) then abort(Error(Reason.Name(value)))
    owner(value).let: other => abort(Error(Reason.ValueExists(other.name, value)))
    val above = parent.or(found.root)
    Store.sound(found.value(above))

    val entries = settings.axis.map: entry =>
      if entry.name != axis then entry else
        val translation: Optional[Boolean] =
          if entry.kind == t"language" && Settings.language(value) != Settings.language(above)
          then true
          else Unset

        entry.copy(value = entry.value + List(ValueEntry(value, above, translation)))

    configure(settings.copy(axis = entries))

  // The store in which a stale hunk on an axis shows its own lines, or its storage parent's.
  // The choice is part of every point's content, so it is made before any hunk is stored.
  def prefer(axis: Text, parent: Boolean): Workspace raises Store.Error raises Error =
    if !settings.axis.exists(_.name == axis) then abort(Store.Error(Store.Reason.Unknown(axis)))

    val stored = dossiers.values.to[List].exists: dossier =>
      dossier.folios.values.to[List].exists:
        case Folio.Delta(name, hunks) => name == axis && !hunks.nil
        case _                        => false

    val name = if parent then t"parent" else t"own"
    val former = settings.axis.filter(_.name == axis).head.let(_.prefer).or(name)
    if stored && former != name then abort(Error(Reason.Preference(axis)))

    configure:
      settings.copy(axis = settings.axis.map: entry =>
        if entry.name == axis then entry.copy(prefer = name) else entry)

  // Derives every document at the points given, each document in a task of its own, so that
  // whatever then reads them there finds them derived: no document's content depends on
  // another's. Nothing is refused here; what is wrong with a document is found when it is read.
  def prepare(points: List[Point])(using Monitor): Unit =
    val tasks = documents.map: document =>
      val held = dossier(document)
      async(points.each { point => safely(held.rendition(point)) })

    tasks.each: task => safely(task.await())

  // Everything which makes the store invalid, as `faults` gives it. The documents are derived
  // a few at a time, each in a task of its own, and what is derived of one is given up when
  // its points have been looked at: every document derived at every point, and all of it held
  // at once, is more than a store of many points leaves room for.
  def survey()(using Monitor): List[Store.Error] raises Store.Error =
    def apart(document: Text): List[Store.Error] | Store.Error =
      val dossiers = Map[Text, Dossier]().define(document, dossier(document).copy())
      val alone = new Store(settings, space, dossiers, Nil, upstream)

      recover:
        case error: Store.Error => error

      . protect(alone.unplaced(List(document)))

    val found: List[List[Store.Error] | Store.Error] =
      List.from(documents.stdlib.grouped(Workspace.abreast)).bind: group =>
        val tasks = List.from(group).map: document => (document, async(apart(document)))
        tasks.map: (document, task) => safely(task.await()).or(apart(document))

    val unplaced = found.bind:
      case error: Store.Error => abort(error)
      case faults: List[Store.Error] @unchecked => faults

    omitted + descents + unplaced

  // The store in which a document has the dossier given, with nothing written.
  def supposing(document: Text, dossier: Dossier): Workspace =
    holding(dossiers.define(document, dossier))
