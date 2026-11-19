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

        case Reason.Preference(axis) =>
          m"the axis $axis has hunks stored on it, so what it prefers cannot change" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Name(_)                => Orthant.bolt(2, 4)
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

  private def folder(root: Text): Text = t"$root/.orthant"
  private def config(root: Text): Text = t"$root/.orthant/config.tel"

  // The directory, at or above the one given, which has a store in it.
  def find(directory: Text): Text raises Store.Error = Store.find(Disk.path(directory)).encode

  // Creates an empty store, with no axes, whose working tree is the directory `tree`.
  def init(root: Text, tree: Text): Workspace raises Store.Error raises Error =
    if Disk.exists(config(root)) then abort(Error(Reason.Exists(root)))
    val staging = t"${folder(root)}/work/staging"
    val settings = Settings(Settings.format, tree, Nil)
    Disk.write(t"${folder(root)}/.gitattributes", t"* -text\n", staging)
    Disk.write(t"${folder(root)}/.gitignore", t"/work/\n", staging)
    val ignore = Disk.read(t"$root/.gitignore").or(t"")

    if !Store.lines(ignore).has(t"/$tree/") then
      val ignored = Store.text(Store.lines(ignore) + List(t"/$tree/"))
      Disk.write(t"$root/.gitignore", ignored, staging)

    Workspace(root, new Store(settings, Store.sound(settings.space), Map())).configure(settings)

  // The store beneath `root`, and everything which is wrong with what it holds. A file which
  // cannot be read as a record is left out.
  def scan(root: Text): (Workspace, List[Store.Error]) raises Store.Error =
    val store = Store.inspect(Disk.path(root))
    (Workspace(root, store), store.faults)

  // The store beneath `root`, which must have nothing wrong with it.
  def load(root: Text): Workspace raises Store.Error =
    val (store, faults) = scan(root)
    faults.head.let(abort(_))
    store

// A store on disk: the directory `root` which has `.orthant` in it, and the store as it was
// read from there, or as it has since been made.
case class Workspace(root: Text, held: Store)
extends Store(held.settings, held.space, held.dossiers, held.omitted):
  import Workspace.{Error, Reason}

  val folder: Text = t"$root/.orthant"
  val data: Text = t"$folder/data"
  val work: Text = t"$folder/work"
  val staging: Text = t"$work/staging"
  val lock: Text = t"$work/lock"
  val tree: Text = t"$root/${settings.tree}"

  private def file(point: Point, document: Text, holding: Optional[Holding]): Optional[Text] =
    holding.let:
      case Holding.Base(_) => t"$data/${name(point)}/$document"
      case _               => t"$data/${name(point)}/$document.delta"

  private def holding(dossiers: Map[Text, Dossier]): Workspace =
    Workspace(root, new Store(settings, space, dossiers, omitted))

  // The point which `from` becomes when moved to each value given, as `axis=value`, or as a
  // value alone where one axis has it.
  def locate(from: Point, values: List[Text]): Point raises Store.Error = point(values, from)

  // The store in which a document has the dossier given; the files of each point whose holding
  // differs are written, or deleted.
  def save(document: Text, updated: Dossier): Workspace raises Store.Error =
    val former = dossier(document)

    points.each: point =>
      val old = former.holdings.at(point)
      val holding = updated.holdings.at(point)

      if old != holding then
        val target = file(point, document, holding)

        file(point, document, old).let: path => if path != target then Disk.remove(path, data)

        target.let: path =>
          val content: List[Text] = holding match
            case Holding.Base(lines)    => lines
            case delta: Holding.Delta   => Delta.serialize(delta)
            case absent: Holding.Absent => Delta.serialize(absent)
            case _                      => Nil

          Disk.write(path, Store.text(content), staging)

    val present = updated.holdings.keys.to[List].nil
    holding(if present then dossiers.omit(document) else dossiers.define(document, updated))

  // The store with a new configuration, which is written.
  def configure(settings: Settings): Workspace raises Store.Error =
    Disk.write(t"$folder/config.tel", Store.text(Store.lines(settings.in[Tel].show)), staging)
    val space = Store.sound(settings.space)

    // Each dossier is of the space it was read in, so it is made again in the new one.
    val moved = documents.fold(Map[Text, Dossier]()): (moved, document) =>
      moved.define(document, Dossier(space, dossier(document).holdings))

    Workspace(root, new Store(settings, space, moved, omitted))

  // The store with a new axis, last in precedence, which has the one value `value`.
  def axis(name: Text, value: Text, kind: Text, pattern: Optional[Text] = Unset)
  :   Workspace raises Store.Error raises Error =

    if !Store.valid(name) || name.contains(t"/") then abort(Error(Reason.Name(name)))
    if !Store.valid(value) then abort(Error(Reason.Name(value)))
    if settings.axis.exists(_.name == name) then abort(Error(Reason.AxisExists(name)))
    val entry = AxisEntry(name, kind, t"own", List(ValueEntry(value)), pattern)
    configure(settings.copy(axis = settings.axis + List(entry)))

  // The store with a new value on an axis, beneath `parent` or else beneath the axis's root.
  def value(axis: Text, value: Text, parent: Optional[Text])
  :   Workspace raises Store.Error raises Error =

    if !Store.valid(value) then abort(Error(Reason.Name(value)))
    val unknown = Store.Error(Store.Reason.Unknown(axis))
    val found = space.axes.filter(_.name == axis).head.or(abort(unknown))
    if found.has(value) then abort(Error(Reason.ValueExists(axis, value)))
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
      dossier.holdings.values.to[List].exists:
        case Holding.Delta(name, hunks) => name == axis && !hunks.nil
        case _                          => false

    val name = if parent then t"parent" else t"own"
    val former = settings.axis.filter(_.name == axis).head.let(_.prefer).or(name)
    if stored && former != name then abort(Error(Reason.Preference(axis)))

    configure:
      settings.copy(axis = settings.axis.map: entry =>
        if entry.name == axis then entry.copy(prefer = name) else entry)

  // The store in which a document has the dossier given, with nothing written.
  def supposing(document: Text, dossier: Dossier): Workspace =
    holding(dossiers.define(document, dossier))
