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

// A reader of a store, as spec/store.md §12 defines one. A store is opened from its home, which
// is of any type that is `Perusable`, and is then a value: every answer it gives is a function of
// what was read when it was opened, so all of them are of one state of the store. Nothing here
// writes to a store.
object Store:
  enum Reason:
    case Missing(directory: Text)
    case Config(detail: Text)
    case Access(file: Text, detail: Text)
    case Axes(detail: Text, code: fulminate.Bolt)
    case Ambiguous(value: Text)
    case Unknown(value: Text)
    case Folder(name: Text)
    case Both(file: Text)
    case Storage(file: Text, axis: Text)
    case Format(file: Text, detail: Text, code: fulminate.Bolt)
    case Unplaced(document: Text, point: Text)
    case Wanting(count: Int, point: Text)
    case Sourced(file: Text)
    case Descent(axis: Text, value: Text, parent: Text)
    case Source(repository: Text, detail: Text)

  case class Error(reason: Reason)(using Diagnostics)
  extends fulminate.Error
    ( reason match
        case Reason.Missing(directory) =>
          m"there is no Orthant store in $directory or above it"

        case Reason.Config(detail) =>
          m"the configuration could not be read: $detail"

        case Reason.Access(file, detail) =>
          m"the file $file could not be read or written: $detail"

        case Reason.Axes(detail, _) =>
          m"$detail"

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

        case Reason.Wanting(count, point) =>
          m"""
            $count of the documents at $point have stale, conflicting or untranslated lines;
            `orthant status` lists them
          """

        case Reason.Unplaced(document, point) =>
          m"a hunk of $document at $point does not have its lines exactly once"

        case Reason.Sourced(file) =>
          m"the store holds $file at a point whose content is the source's"

        case Reason.Descent(axis, value, parent) =>
          m"on the axis $axis, the commit of $value does not descend from that of $parent"

        case Reason.Source(repository, detail) =>
          m"the source's repository $repository could not be read: $detail" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Both(_)            => Orthant.bolt(1, 5)
      case Reason.Storage(_, _)      => Orthant.bolt(1, 6)
      case Reason.Folder(_)          => Orthant.bolt(1, 7)
      case Reason.Unplaced(_, _)     => Orthant.bolt(1, 8)
      case Reason.Sourced(_)         => Orthant.bolt(1, 9)
      case Reason.Descent(_, _, _)   => Orthant.bolt(7, 6)
      case Reason.Source(_, _)       => Orthant.bolt(7, 5)
      case Reason.Format(_, _, code) => code
      case Reason.Axes(_, code)      => code
      case Reason.Ambiguous(_)       => Orthant.bolt(2, 7)
      case Reason.Unknown(_)         => Orthant.bolt(2, 8)
      case Reason.Missing(_)         => Orthant.bolt(4, 1)
      case Reason.Access(_, _)       => Orthant.bolt(4, 5)
      case Reason.Wanting(_, _)      => Orthant.bolt(4, 11)
      case Reason.Config(_)          => Orthant.bolt(5, 1)

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

  // Whether a value may have this name; an axis may, if it has no `/` besides.
  def valid(name: Text): Boolean = name match
    case r"[A-Za-z0-9][A-Za-z0-9.+/-]*" => name != t"root"
    case _                              => false

  final val kinds: List[Text] = List(t"language", t"custom", t"git")
  final val preferences: List[Text] = List(t"own", t"parent")

  private val config: Text = t".orthant/config.tel"
  private val data: Text = t".orthant/data"

  // An error of the axes is an error of the store which has them, with the same code.
  private[orthant] def sound[result](block: Tactic[Axis.Error] ?=> result): result raises Error =
    mitigate:
      case error: Axis.Error => Error(Reason.Axes(Orthant.prose(error), error.bolt))

    . protect(block)

  // The text of a file, which must be well-formed UTF-8, and have no byte order mark: one is
  // decoded as `U+FEFF`, and would otherwise be taken for the start of the first line.
  private def decode(file: Text, bytes: Data): Text raises Error =
    val text: Text =
      mitigate:
        case error: Charset.Error => Error(Reason.Access(file, Orthant.prose(error)))

      . protect:
          import soundness.textSanitizers.strictSanitizer
          soundness.charsets.utf8Charset.decoded(bytes)

    if !text.starts(t"\ufeff") then text
    else abort(Error(Reason.Access(file, t"it begins with a byte order mark")))

  // The configuration, which is refused unless it is all that §6.2 and §7 of the specification
  // require: nothing is made of one which is not.
  private[orthant] def configuration[source: Perusable](home: source): Settings raises Error =
    val file = t"${source.name(home)}/$config"
    val missing = Error(Reason.Missing(source.name(home)))
    val text = decode(file, source.data(home, config).or(abort(missing)))

    val settings: Settings =
      mitigate:
        case error: Tel.Error => Error(Reason.Config(Orthant.prose(error)))

      . protect(text.read[Settings in Tel])

    def refuse(detail: Text): Nothing = abort(Error(Reason.Config(detail)))

    // A store of another version is not read for what can be made of it.
    if settings.format != Settings.format && settings.format != Settings.sourced
    then refuse(t"the store is of format ${settings.format}, and not ${Settings.format}")

    if settings.format == Settings.format then settings.axis.filter(_.source.present).each: entry =>
      refuse(t"the axis ${entry.name} has a source, which a store of format 1 cannot have")

    // A name which two axes had, or two values of an axis, would name neither.
    def twice(names: List[Text]): Optional[Text] =
      val counts = names.stdlib.groupBy { name => name }.view.mapValues(_.size).toMap
      names.filter { name => counts(name) > 1 }.head

    twice(settings.axis.map(_.name)).let: name => refuse(t"more than one axis is named $name")

    settings.axis.each: entry =>
      val axis = entry.name

      if !valid(axis) || axis.contains(t"/") then refuse(t"$axis is not a valid name for an axis")

      if !kinds.has(entry.kind)
      then refuse(t"the axis $axis is of the kind ${entry.kind}, which is not a kind of axis")

      if !preferences.has(entry.prefer)
      then refuse(t"the axis $axis prefers ${entry.prefer}, which is neither own nor parent")

      twice(entry.value.map(_.name)).let: name =>
        refuse(t"the axis $axis has more than one value named $name")

      entry.value.each: value =>
        if !valid(value.name)
        then refuse(t"${value.name} is not a valid name for a value of the axis $axis")

    // A sourced axis is the last, so that nothing is ever derived along it; each of its values
    // is a commit, which never moves.
    settings.axis.filter(_.source.present).each: entry =>
      val axis = entry.name

      if settings.axis.last.let(_.name) != axis
      then refuse(t"the axis $axis has a source, but is not the last axis")

      entry.value.each: value =>
        if value.commit.absent
        then refuse(t"the value ${value.name} of the axis $axis, which has a source, has no commit")

        if value.branch.or(false)
        then refuse(t"the value ${value.name} of the axis $axis, which has a source, is a branch")

    settings

  // Where a document's content comes from at the source points of a store with a sourced axis:
  // the file of its name, beneath the source's prefix, in the commit of each value. A file
  // which is not well-formed UTF-8 is not a document.
  private[orthant] def source(settings: Settings, upstream: Optional[Upstream], document: Text)
  :   Optional[Source] =

    settings.axis.filter(_.source.present).head.let: entry =>
      val path = entry.source.let(_.prefix).let { prefix => t"$prefix/$document" }.or(document)

      new Source:
        def content(value: Text): Optional[List[Text]] raises Axis.Error =
          def failed(detail: Text): Axis.Error =
            Axis.Error(entry.name, Axis.Reason.Source(value, detail))

          val repository = upstream.or(abort(failed(t"no repository was given to read it from")))
          val found = entry.value.filter(_.name == value).head.let(_.commit)
          val commit = found.or(abort(failed(t"the value has no commit")))

          val bytes: Optional[Data] =
            mitigate:
              case error: Error => failed(Orthant.prose(error))

            . protect(repository.blob(commit, path))

          bytes.let: bytes =>
            safely:
              import soundness.textSanitizers.strictSanitizer
              lines(soundness.charsets.utf8Charset.decoded(bytes))

  // The home, at or above the place given, which has a store in it.
  def find[origin: Ascendable](directory: origin)(using home: origin.Result is Perusable)
  :   origin.Result raises Error =

    val places = origin.ancestors(directory)
    val missing = Error(Reason.Missing(places.head.let(home.name(_)).or(t"")))

    places.filter { place => home.data(place, config).present }.head.or(abort(missing))

  // The store whose home is given, with whatever of its records can be read: a record which
  // cannot be is left out, and is among the store's `omitted`. A store so read says what it
  // left out, and is for finding everything which is wrong with it; `open` is for reading it.
  def inspect[source: Perusable](home: source, upstream: Optional[Upstream] = Unset)
  :   Store raises Error =

    val settings = configuration(home)
    val space = sound(settings.space)
    val base = source.name(home)

    def fresh(document: Text): Dossier =
      Dossier(space, Map(), Store.source(settings, upstream, document))

    def attempt[result](block: Tactic[Error] ?=> result): result | Error =
      recover:
        case error: Error => error

      . protect(block)

    // Hidden entries are passed over at every depth, and so is whatever is neither a directory
    // nor a regular file.
    def entries(directory: Text, kind: Perusable.Kind): List[Text] =
      source.entries(home, directory).filter { entry => entry.kind == kind }.map(_.name)
      . filter(!_.starts(t"."))

    def files(directory: Text, prefix: Text): List[Text] =
      val deeper = entries(directory, Perusable.Kind.Directory).bind: name =>
        files(t"$directory/$name", t"$prefix$name/")

      entries(directory, Perusable.Kind.File).map { name => t"$prefix$name" } + deeper

    type State = (Map[Text, Dossier], List[Error])
    val folders = entries(data, Perusable.Kind.Directory).order: name => name

    val (dossiers, faults) = folders.fold((Map(), Nil): State): (state, name) =>
      attempt(point(space, name)) match
        case error: Error =>
          (state(0), error :: state(1))

        case point: Point =>
          files(t"$data/$name", t"").order { file => file }.fold(state): (state, file) =>
            val (dossiers, faults) = state
            val path = t"$data/$name/$file"
            val shown = t"$base/$path"
            val delta = point != space.root && file.ends(t".delta")
            val document = if delta then file.keep(file.length - 6) else file
            val dossier = dossiers.at(document).or(fresh(document))

            val folio: Folio | Error = attempt:
              val gone = Error(Reason.Access(shown, t"it is no longer there"))
              val content = lines(decode(shown, source.data(home, path).or(abort(gone))))
              if dossier.folios.defines(point) then abort(Error(Reason.Both(shown)))
              if space.source(point) then abort(Error(Reason.Sourced(shown)))

              if !delta then Folio.Base(content) else
                val folio = parse(shown, content)

                val axis = folio match
                  case Folio.Delta(axis, _) => axis
                  case Folio.Absent(axis)   => axis
                  case _                    => t""

                if space.storageAxis(point).let(_.name) != axis
                then abort(Error(Reason.Storage(shown, axis)))

                folio

            folio match
              case error: Error =>
                (dossiers, error :: faults)

              case folio: Folio =>
                (dossiers.define(document, dossier.hold(point, folio)), faults)

    new Store(settings, space, dossiers, faults.reverse, upstream)

  // The store whose home is given. It is refused if it is invalid in any way which can be found
  // without deriving content: all that `faults` gives but a hunk which has no one place.
  def open[source: Perusable](home: source, upstream: Optional[Upstream] = Unset)
  :   Store raises Error =

    val store = inspect(home, upstream)
    store.omitted.head.let(abort(_))
    store

  private def parse(file: Text, content: List[Text]): Folio raises Error =
    mitigate:
      case error: Delta.Error => Error(Reason.Format(file, Orthant.prose(error), error.bolt))

    . protect:
        Delta.parse(content) match
          case delta: Folio.Delta   => delta
          case absent: Folio.Absent => absent

// A store, as it was when it was read: the configuration, the space its axes span, and every
// document's dossier, by the document's path. `omitted` is what was wrong with the records which
// were left out of it, and is empty in a store which was opened.
class Store
  ( val settings: Settings,
    val space:    Space,
    val dossiers: Map[Text, Dossier],
    val omitted:  List[Store.Error]  = Nil,
    val upstream: Optional[Upstream] = Unset ):

  import Store.{Error, Reason, sound}

  def format: Int = settings.format

  // The axes, in order. What the configuration says of an axis besides — its kind, its pattern,
  // and the labels, title, commit and branch of each value — is in its `entry`.
  def axes: List[Axis] = space.axes
  def entry(axis: Text): Optional[AxisEntry] = settings.axis.filter(_.name == axis).head

  // Every point, each after all the points it is below.
  def points: List[Point] raises Error = sound(space.points)

  def storageParent(point: Point): Optional[Point] raises Error = sound(space.storageParent(point))

  // The points whose hunks a point inherits, from the most specific, which is itself, to the
  // least: the deeper on the earliest axis on which two of them differ.
  def origins(point: Point): List[Point] raises Error = sound:
    space.axes.reverse.fold(space.origins(point)): (points, axis) =>
      points.order: point => -axis.depth(point(axis))

  def documents: List[Text] = dossiers.keys.to[List].order: document => document

  // A document which the store has no record of has a dossier all the same: where an axis has
  // a source, it is what the source has.
  def dossier(document: Text): Dossier =
    dossiers.at(document).or(Dossier(space, Map(), Store.source(settings, upstream, document)))

  def name(point: Point): Text = Store.name(space, point)

  // The point which `from` becomes when moved to each value a term names, in turn: a term is
  // `axis=value`, or a value alone where one axis has it.
  def point(terms: List[Text], from: Optional[Point] = Unset): Point raises Error =
    terms.fold(from.or(space.root)): (point, text) =>
      val (axis, value) = term(text)
      point.move(axis, value)

  // The axis, and the value of it, which a term names.
  def term(text: Text): (Axis, Text) raises Error =
    text.cut(t"=") match
      case axis :: value :: Nil =>
        val found = space.axes.filter(_.name == axis).head.or(abort(Error(Reason.Unknown(axis))))
        val target = named(axis, value).or(value)
        sound(found.value(target))
        (found, target)

      case _ =>
        space.axes.filter { axis => named(axis.name, text).present } match
          case found :: Nil => (found, named(found.name, text).or(text))
          case Nil          => abort(Error(Reason.Unknown(text)))
          case _            => abort(Error(Reason.Ambiguous(text)))

  // The points which are at each value given, or below it; every point, if none is given.
  def within(terms: List[Text]): List[Point] raises Error =
    val found = terms.map(term(_))

    def outside(point: Point): Boolean =
      found.exists: (axis, value) => !sound(axis.below(point(axis), value))

    points.filter(!outside(_))

  // The value of an axis which a term names: by its own name, or by a tag which labels it.
  def named(axis: Text, term: Text): Optional[Text] = entry(axis).let: entry =>
    val labelled = entry.value.filter(_.labels.has(term)).head
    entry.value.filter(_.name == term).head.or(labelled).let(_.name)

  // The name by which a value is shown.
  def title(axis: Text, value: Text): Text =
    entry(axis).let(_.value.filter(_.name == value).head).let(_.title).or(value)

  // All that is derived of a document at a point: its content, the reference it is derived
  // against, each placement, and the issues. It is absent where the document is.
  def rendition(document: Text, point: Point): Optional[Rendition] raises Error =
    sound(dossier(document).rendition(point))

  private def misplaced(document: Text, point: Point): Boolean raises Error =
    sound(dossier(document).issues(point)).exists:
      case Issue.Orphan(_, _)    => true
      case Issue.Ambiguous(_, _) => true
      case _                     => false

  // The content of a document at a point, as its lines. A content which would come of a hunk
  // with no one place is not given: `rendition` gives it, with the issue.
  def content(document: Text, point: Point): Optional[List[Text]] raises Error =
    if misplaced(document, point)
    then abort(Error(Reason.Unplaced(document, name(point))))
    else rendition(document, point).let(_.lines)

  // The text of that content, in UTF-8.
  def text(document: Text, point: Point): Optional[Data] raises Error =
    content(document, point).let: lines => utf8Codepage.encoded(Store.text(lines))

  // Each point at which one of the documents given has a hunk with no one place. Finding them
  // derives every one of those documents at every point.
  def unplaced(documents: List[Text]): List[Error] raises Error =
    val every = points

    documents.bind: document =>
      every.bind: point =>
        val found = sound(dossier(document).issues(point)).exists:
          case Issue.Orphan(origin, _)    => origin == point
          case Issue.Ambiguous(origin, _) => origin == point
          case _                          => false

        if found then List(Error(Reason.Unplaced(document, name(point)))) else Nil

  // Everything which makes the store invalid; it is valid if there is nothing.
  def faults: List[Error] raises Error = omitted + descents + unplaced(documents)

  // Each value of a sourced axis whose commit does not descend from its parent's: the axis's
  // tree must be one which the source's history allows.
  def descents: List[Error] raises Error =
    settings.axis.filter(_.source.present).bind: entry =>
      val repository = upstream.or:
        abort(Error(Reason.Source(entry.name, t"no repository was given to read it from")))

      entry.value.bind: value =>
        val above = value.parent.let: parent => entry.value.filter(_.name == parent).head

        above.lay(Nil): above =>
          val descends = value.commit.lay(false): commit =>
            above.commit.lay(false)(repository.ancestor(_, commit))

          if descends then Nil else List(Error(Reason.Descent(entry.name, value.name, above.name)))

  // The work in a document at a point is asked for by every point which inherits from it, and
  // is found once.
  private val works: scala.collection.mutable.HashMap[(Text, Point), Survey.Work] =
    scala.collection.mutable.HashMap()

  private[orthant] def recall(document: Text, point: Point)(work: => Survey.Work): Survey.Work =
    works.get((document, point)).getOrElse:
      val found = work
      works((document, point)) = found
      found

  def work(document: Text, point: Point): Survey.Work raises Error =
    sound(Survey.work(this, document, point))

  def work(point: Point): Survey.Sum raises Error = sound(Survey.sum(this, point))

  // The work in a document which is best done at this point, and not at one above it.
  def ownWork(document: Text, point: Point): Survey.Work raises Error =
    sound(Survey.own(this, document, point))

  // The document beside its reference, every line marked with a sigil.
  def view(document: Text, point: Point): Optional[List[Text]] raises Error =
    sound(Indirect.view(dossier(document), point))

  // Writes the text of every document which has a content at a point beneath a directory, which
  // is outside the store, and gives their number. If `strict` is set, nothing is written while
  // any document wants attention there.
  def publish[target: Depositable](point: Point, directory: target, strict: Boolean = false)
  :   Int raises Error =

    val texts: List[(Text, Data)] = documents.bind: document =>
      text(document, point).lay(Nil): data => List((document, data))

    if strict then
      val wanting = documents.count: document => work(document, point).wanting
      if wanting > 0 then abort(Error(Reason.Wanting(wanting, name(point))))

    texts.each: (document, data) => target.deposit(directory, document, data)
    texts.size
