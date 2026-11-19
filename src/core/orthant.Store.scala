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

import proscenium.{List, Nil, Map, `::`}
import soundness.*
import soundness.codepages.utf8Codepage
import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.sortingAlgorithms.timsort

object Store:
  enum Reason:
    case Missing(directory: Text)
    case Exists(directory: Text)
    case Config(detail: Text)
    case Access(file: Text, detail: Text)
    case Name(name: Text)
    case AxisExists(axis: Text)
    case ValueExists(axis: Text, value: Text)
    case Ambiguous(value: Text)
    case Unknown(value: Text)
    case Folder(name: Text)
    case Both(file: Text)
    case Storage(file: Text, axis: Text)
    case Format(file: Text, detail: Text, code: fulminate.Bolt)
    case Unplaced(document: Text, point: Text)
    case Edited(count: Int)
    case Moved(document: Text)
    case Suffix(document: Text)
    case Refused(document: Text, point: Text, detail: Text, code: fulminate.Bolt)
    case Rootless(point: Text)
    case Wanting(count: Int, point: Text)
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
        case Reason.Missing(directory) =>
          m"there is no Orthant store in $directory or above it"

        case Reason.Exists(directory) =>
          m"there is already an Orthant store in $directory"

        case Reason.Config(detail) =>
          m"the configuration could not be read: $detail"

        case Reason.Access(file, detail) =>
          m"the file $file could not be read or written: $detail"

        case Reason.Name(name) =>
          m"$name is not a valid name for an axis or a value"

        case Reason.AxisExists(axis) =>
          m"the axis $axis already exists"

        case Reason.ValueExists(axis, value) =>
          m"the axis $axis already has the value $value"

        case Reason.Ambiguous(value) =>
          m"more than one axis has the value $value; write it as axis=$value"

        case Reason.Unknown(value) =>
          m"no axis has the value $value"

        case Reason.Folder(name) =>
          m"the directory $name in the store does not name a point"

        case Reason.Both(file) =>
          m"the store holds both $file and its delta"

        case Reason.Storage(file, axis) =>
          m"the delta $file is written against the axis $axis, which is not its storage axis"

        case Reason.Format(file, detail, _) =>
          m"$file: $detail"

        case Reason.Unplaced(document, point) =>
          m"a hunk of $document at $point does not have its lines exactly once"

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

        case Reason.Wanting(count, point) =>
          m"""
            $count of the documents at $point have stale, conflicting or untranslated lines;
            `orthant status` lists them
          """

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
      case Reason.Both(_)                => Orthant.bolt(1, 5)
      case Reason.Storage(_, _)          => Orthant.bolt(1, 6)
      case Reason.Folder(_)              => Orthant.bolt(1, 7)
      case Reason.Unplaced(_, _)         => Orthant.bolt(1, 8)
      case Reason.Format(_, _, code)     => code
      case Reason.Name(_)                => Orthant.bolt(2, 4)
      case Reason.AxisExists(_)          => Orthant.bolt(2, 5)
      case Reason.ValueExists(_, _)      => Orthant.bolt(2, 6)
      case Reason.Ambiguous(_)           => Orthant.bolt(2, 7)
      case Reason.Unknown(_)             => Orthant.bolt(2, 8)
      case Reason.Preference(_)          => Orthant.bolt(2, 9)
      case Reason.Rootless(_)            => Orthant.bolt(4, 10)
      case Reason.Wanting(_, _)          => Orthant.bolt(4, 11)
      case Reason.Clash(_)               => Orthant.bolt(4, 12)
      case Reason.Against(_, _)          => Orthant.bolt(4, 13)
      case Reason.Unmoved(_)             => Orthant.bolt(4, 14)
      case Reason.Refused(_, _, _, code) => code
      case Reason.Missing(_)             => Orthant.bolt(4, 1)
      case Reason.Edited(_)              => Orthant.bolt(4, 2)
      case Reason.Moved(_)               => Orthant.bolt(4, 3)
      case Reason.Suffix(_)              => Orthant.bolt(4, 4)
      case Reason.Access(_, _)           => Orthant.bolt(4, 5)
      case Reason.Config(_)              => Orthant.bolt(5, 1)
      case Reason.Repository(_)          => Orthant.bolt(7, 1)
      case Reason.Lineage(_, _)          => Orthant.bolt(7, 2)
      case Reason.Branch(_)              => Orthant.bolt(7, 3)
      case Reason.Kind(_)                => Orthant.bolt(7, 4)
      case Reason.Exists(_)              => Orthant.bolt(5, 2)

  // The lines of a document's text. Each line ends with a newline; a last line without one is
  // a line all the same, and is given one when it is written.
  def lines(text: Text): List[Text] =
    val parts = text.cut(t"\n")
    val count = parts.count: part => true
    if parts.last == t"" then parts.excerpt(0, count - 1) else parts

  def text(lines: List[Text]): Text = lines.map { line => t"$line\n" }.join

  // The name of the directory which holds a point's records.
  def name(space: Space, point: Point): Text =
    val pairs = space.axes.filter { axis => point(axis) != axis.root }.map: axis =>
      t"${axis.name}=${point(axis).cut(t"/").join(t"_")}"

    if pairs.nil then t"root" else pairs.join(t",")

  def point(space: Space, name: Text): Point raises Error =
    def invalid: Nothing = abort(Error(Reason.Folder(name)))

    // A pair gives a value other than the root, so a point which is no longer at an axis's root
    // has already had its value there from an earlier pair: no axis is named twice.
    if name == t"root" then space.root else name.cut(t",").fold(space.root): (point, pair) =>
      pair.cut(t"=") match
        case axis :: value :: Nil =>
          val found = space.axes.filter(_.name == axis).head.or(invalid)
          val value2 = value.cut(t"_").join(t"/")
          if !found.has(value2) || value2 == found.root then invalid
          if point(found) != found.root then invalid
          point.move(found, value2)

        case _ =>
          invalid

  def valid(name: Text): Boolean = name match
    case r"[A-Za-z0-9][A-Za-z0-9.+/-]*" => name != t"root"
    case _                              => false

  private def folder(root: Text): Text = t"$root/.orthant"
  private def config(root: Text): Text = t"$root/.orthant/config.tel"

  // The directory, at or above the one given, which has a store in it.
  def find(directory: Text): Text raises Error =
    def recur(current: Text): Text =
      if Disk.exists(config(current)) then current
      else if current == t"" then abort(Error(Reason.Missing(directory)))
      else recur(Disk.parent(current))

    recur(directory)

  // Creates an empty store, with no axes, whose working tree is the directory `tree`.
  def init(root: Text, tree: Text): Store raises Error raises Axis.Error =
    if Disk.exists(config(root)) then abort(Error(Reason.Exists(root)))
    val staging = t"${folder(root)}/work/staging"
    val settings = Settings(Settings.format, tree, Nil)
    Disk.write(t"${folder(root)}/.gitattributes", t"* -text\n", staging)
    Disk.write(t"${folder(root)}/.gitignore", t"/work/\n", staging)
    val ignore = Disk.read(t"$root/.gitignore").or(t"")

    if !lines(ignore).has(t"/$tree/")
    then Disk.write(t"$root/.gitignore", text(lines(ignore) + List(t"/$tree/")), staging)

    Store(root, settings, settings.space, Map()).configure(settings)

  private def parse(file: Text, content: List[Text]): Holding raises Error =
    mitigate:
      case error: Delta.Error => Error(Reason.Format(file, Orthant.prose(error), error.bolt))

    . protect:
        Delta.parse(content) match
          case delta: Holding.Delta   => delta
          case absent: Holding.Absent => absent

  // The store beneath `root`, and everything which is wrong with what it holds. A file which
  // cannot be read as a record is left out.
  def scan(root: Text): (Store, List[Error]) raises Error raises Axis.Error =
    val settings: Settings =
      mitigate:
        case error: Tel.Error => Error(Reason.Config(Orthant.prose(error)))

      . protect:
          Disk.read(config(root)).or(abort(Error(Reason.Missing(root)))).read[Settings in Tel]

    // A store of another version is not read for what can be made of it.
    if settings.format != Settings.format then
      val detail = t"the store is of format ${settings.format}, and not ${Settings.format}"
      abort(Error(Reason.Config(detail)))

    // A name which two axes had, or two values of an axis, would name neither.
    def twice(names: List[Text]): Optional[Text] =
      names.filter { name => names.count(_ == name) > 1 }.head

    twice(settings.axis.map(_.name)).let: name =>
      abort(Error(Reason.Config(t"more than one axis is named $name")))

    settings.axis.each: entry =>
      twice(entry.value.map(_.name)).let: name =>
        abort(Error(Reason.Config(t"the axis ${entry.name} has more than one value named $name")))

    val space = settings.space
    val data = t"${folder(root)}/data"

    def attempt[result](block: Tactic[Error] ?=> result): result | Error =
      recover:
        case error: Error => error

      . protect(block)

    type State = (Map[Text, Dossier], List[Error])

    val (dossiers, faults) = Disk.directories(data).fold((Map(), Nil): State): (state, name) =>
      attempt(point(space, name)) match
        case error: Error =>
          (state(0), error :: state(1))

        case point: Point =>
          Disk.files(t"$data/$name").fold(state): (state, file) =>
            val (dossiers, faults) = state
            val path = t"$data/$name/$file"
            val delta = point != space.root && file.ends(t".delta")
            val document = if delta then file.keep(file.length - 6) else file
            val dossier = dossiers.at(document).or(Dossier(space))

            val holding: Holding | Error = attempt:
              val content = lines(Disk.read(path).or(t""))
              if dossier.holdings.defines(point) then abort(Error(Reason.Both(path)))

              if !delta then Holding.Base(content) else
                val holding = parse(path, content)

                val axis = holding match
                  case Holding.Delta(axis, _) => axis
                  case Holding.Absent(axis)   => axis
                  case _                      => t""

                if space.storageAxis(point).let(_.name) != axis
                then abort(Error(Reason.Storage(path, axis)))

                holding

            holding match
              case error: Error =>
                (dossiers, error :: faults)

              case holding: Holding =>
                (dossiers.define(document, dossier.hold(point, holding)), faults)

    val store = Store(root, settings, space, dossiers)

    val unplaced: List[Error] = store.documents.bind: document =>
      space.points.bind: point =>
        val found = store.dossier(document).issues(point).exists:
          case Issue.Orphan(origin, _)    => origin == point
          case Issue.Ambiguous(origin, _) => origin == point
          case _                          => false

        if found then List(Error(Reason.Unplaced(document, name(space, point)))) else Nil

    (store, faults.reverse + unplaced)

  // The store beneath `root`, which must have nothing wrong with it.
  def load(root: Text): Store raises Error raises Axis.Error =
    val (store, faults) = scan(root)
    faults.head.let(abort(_))
    store

// A store on disk: the directory `root` which has `.orthant` in it, the configuration, and every
// document's dossier, by the document's path in the working tree.
case class Store(root: Text, settings: Settings, space: Space, dossiers: Map[Text, Dossier]):
  import Store.{Error, Reason}

  val folder: Text = t"$root/.orthant"
  val data: Text = t"$folder/data"
  val work: Text = t"$folder/work"
  val staging: Text = t"$work/staging"
  val lock: Text = t"$work/lock"
  val tree: Text = t"$root/${settings.tree}"

  def documents: List[Text] = dossiers.keys.to[List].order: document => document
  def dossier(document: Text): Dossier = dossiers.at(document).or(Dossier(space))
  def name(point: Point): Text = Store.name(space, point)

  private def file(point: Point, document: Text, holding: Optional[Holding]): Optional[Text] =
    holding.let:
      case Holding.Base(_) => t"$data/${name(point)}/$document"
      case _               => t"$data/${name(point)}/$document.delta"

  // The store in which a document has the dossier given; the files of each point whose holding
  // differs are written, or deleted.
  def save(document: Text, updated: Dossier): Store raises Error raises Axis.Error =
    val former = dossier(document)

    space.points.each: point =>
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
    copy(dossiers = if present then dossiers.omit(document) else dossiers.define(document, updated))

  // The store with a new configuration, which is written.
  def configure(settings: Settings): Store raises Error raises Axis.Error =
    Disk.write(t"$folder/config.tel", Store.text(Store.lines(settings.in[Tel].show)), staging)
    val space = settings.space

    // Each dossier is of the space it was read in, so it is made again in the new one.
    val moved = documents.fold(Map[Text, Dossier]()): (moved, document) =>
      moved.define(document, Dossier(space, dossier(document).holdings))

    Store(root, settings, space, moved)

  // The store with a new axis, last in precedence, which has the one value `value`.
  def axis(name: Text, value: Text, kind: Text, pattern: Optional[Text] = Unset)
  :   Store raises Error raises Axis.Error =

    if !Store.valid(name) || name.contains(t"/") then abort(Error(Reason.Name(name)))
    if !Store.valid(value) then abort(Error(Reason.Name(value)))
    if settings.axis.exists(_.name == name) then abort(Error(Reason.AxisExists(name)))
    val entry = AxisEntry(name, kind, t"own", List(ValueEntry(value)), pattern)
    configure(settings.copy(axis = settings.axis + List(entry)))

  // The store with a new value on an axis, beneath `parent` or else beneath the axis's root.
  def value(axis: Text, value: Text, parent: Optional[Text])
  :   Store raises Error raises Axis.Error =

    if !Store.valid(value) then abort(Error(Reason.Name(value)))
    val found = space.axes.filter(_.name == axis).head.or(abort(Error(Reason.Unknown(axis))))
    if found.has(value) then abort(Error(Reason.ValueExists(axis, value)))
    val above = parent.or(found.root)
    found.value(above)

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
  def prefer(axis: Text, parent: Boolean): Store raises Error raises Axis.Error =
    if !settings.axis.exists(_.name == axis) then abort(Error(Reason.Unknown(axis)))

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

  // The point which `from` becomes when moved to each value given, as `axis=value`, or as a
  // value alone where one axis has it.
  def locate(from: Point, values: List[Text]): Point raises Error raises Axis.Error =
    values.fold(from): (point, term) =>
      term.cut(t"=") match
        case axis :: value :: Nil =>
          val found = space.axes.filter(_.name == axis).head.or(abort(Error(Reason.Unknown(axis))))
          val target = named(axis, value).or(value)
          found.value(target)
          point.move(found, target)

        case _ =>
          space.axes.filter { axis => named(axis.name, term).present } match
            case found :: Nil => point.move(found, named(found.name, term).or(term))
            case Nil          => abort(Error(Reason.Unknown(term)))
            case _            => abort(Error(Reason.Ambiguous(term)))

  // The axis, and the value of it, which a term names: `axis=value`, or a value alone where
  // one axis has it.
  def term(text: Text): (Axis, Text) raises Error raises Axis.Error =
    text.cut(t"=") match
      case axis :: value :: Nil =>
        val found = space.axes.filter(_.name == axis).head.or(abort(Error(Reason.Unknown(axis))))
        val target = named(axis, value).or(value)
        found.value(target)
        (found, target)

      case _ =>
        space.axes.filter { axis => named(axis.name, text).present } match
          case found :: Nil => (found, named(found.name, text).or(text))
          case Nil          => abort(Error(Reason.Unknown(text)))
          case _            => abort(Error(Reason.Ambiguous(text)))

  // The points which are at each value given, or below it; every point, if none is given.
  def within(values: List[Text]): List[Point] raises Error raises Axis.Error =
    val terms = values.map(term(_))

    def outside(point: Point): Boolean =
      terms.exists: (axis, value) => !axis.below(point(axis), value)

    space.points.filter(!outside(_))

  // The store in which a document has the dossier given, with nothing written.
  def supposing(document: Text, dossier: Dossier): Store =
    copy(dossiers = dossiers.define(document, dossier))

  def entry(axis: Text): Optional[AxisEntry] = settings.axis.filter(_.name == axis).head

  // The value of an axis which a term names: by its own name, or by a tag which labels it.
  def named(axis: Text, term: Text): Optional[Text] = entry(axis).let: entry =>
    val labelled = entry.value.filter(_.labels.has(term)).head
    entry.value.filter(_.name == term).head.or(labelled).let(_.name)

  // The name by which a value is shown.
  def title(axis: Text, value: Text): Text =
    entry(axis).let(_.value.filter(_.name == value).head).let(_.title).or(value)
