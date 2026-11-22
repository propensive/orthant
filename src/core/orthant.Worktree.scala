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
import soundness.codepages.utf8Codepage
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.systems.javaBaseSystem

// The working tree: the documents as they are at one point, in a directory where they are
// edited. Beside the store are kept which point that is, and a pristine copy of each document
// as it was checked out, by which edits are told from what the store gave.
// What is checked out into the working tree, as `.orthant/work/view.tel` holds it: the mode of
// the view, and the value on each axis which is not at its root.
case class Coordinate(axis: Text, value: Text)
case class View(mode: Text, coordinate: List[Coordinate], from: Optional[Text] = Unset)

object Worktree:
  enum Kind:
    case Added, Modified, Missing

  case class Change(document: Text, kind: Kind)

  // What a check-in does to a point: whether the content of any document there changes, and
  // how many more lines are to be translated there, and how many more passages are stale, not
  // counting the work which a point above it has too.
  case class Effect(point: Point, changed: Boolean, pending: Int, stale: Int)

  // What a check-in did: the changes it stored, those it passed over, and what it did to each
  // point it touched.
  case class Tally(stored: List[Change], skipped: List[Change], effects: List[Effect] = Nil)

  // How the working tree shows a document: as its plain content; beside its storage parent's
  // content, each line with a sigil; or against a change to that reference, from what it was
  // at an earlier value of an axis.
  enum Mode:
    case Plain
    case Beside
    case Against(axis: Text, value: Text)

  // Where a check-in is stored, and so which points it reaches: the point checked out and all
  // below it; of those, only the ones with its value on an axis; an ancestor value, and all
  // below that; or, for each change, the ancestor on an axis which first had the lines.
  enum Reach:
    case Below
    case Only(axis: Text)
    case Since(value: Text)
    case Origin(axis: Text)

  private def state(store: Workspace): Text = t"${store.work}/view.tel"
  private def pristine(store: Workspace): Text = t"${store.work}/pristine"

  private def recorded(store: Workspace): Optional[View] raises Store.Error raises Workspace.Error =
    Disk.read(state(store)).let: text =>
      mitigate:
        case error: Tel.Error => Store.Error(Store.Reason.Config(Orthant.prose(error)))

      . protect(text.read[View in Tel])

  // The point which is checked out: the root of every axis, until another is.
  def view(store: Workspace): Point raises Store.Error raises Workspace.Error raises Axis.Error =
    recorded(store).lay(store.space.root): view =>
      store.locate(store.space.root, view.coordinate.map { pair => t"${pair.axis}=${pair.value}" })

  def mode(store: Workspace): Mode raises Store.Error raises Workspace.Error =
    recorded(store).lay(Mode.Plain): view =>
      if view.mode == t"indirect" then Mode.Beside
      else if view.mode != t"change" then Mode.Plain
      else view.from.let(_.cut(t"=")).or(Nil) match
        case axis :: value :: Nil => Mode.Against(axis, value)
        case _                    => Mode.Plain

  private def record(store: Workspace, point: Point, mode: Mode): Unit raises Store.Error =
    val coordinates = store.space.axes.filter { axis => point(axis) != axis.root }.map: axis =>
      Coordinate(axis.name, point(axis))

    val view = mode match
      case Mode.Plain                => View(t"direct", coordinates)
      case Mode.Beside               => View(t"indirect", coordinates)
      case Mode.Against(axis, value) => View(t"change", coordinates, t"$axis=$value")

    Disk.write(state(store), Store.text(Store.lines(view.in[Tel].show)), store.staging)

  private def axis(store: Workspace, name: Text): Axis raises Store.Error =
    store.space.axes.filter(_.name == name).head.or(abort(Store.Error(Store.Reason.Unknown(name))))

  // A view needs a storage parent to be its reference; one against a change needs the change
  // to be from an ancestor of the point's value, on an axis other than its storage axis.
  private def admit(store: Workspace, point: Point, mode: Mode)
  :   Unit raises Store.Error raises Workspace.Error raises Axis.Error =

    val storage = store.space.storageAxis(point)

    mode match
      case Mode.Plain =>
        ()

      case Mode.Beside =>
        if storage.absent then abort(Workspace.Error(Workspace.Reason.Rootless(store.name(point))))

      case Mode.Against(name, value) =>
        if storage.absent then abort(Workspace.Error(Workspace.Reason.Rootless(store.name(point))))
        val found = axis(store, name)

        if storage.let(_.name) == name || !found.has(value) || !found.below(point(found), value)
        then abort(Workspace.Error(Workspace.Reason.Against(name, value)))

  // What the reference of a document's view was, at the earlier value of a view against a
  // change.
  private def former(store: Workspace, dossier: Dossier, point: Point, name: Text, value: Text)
  :   List[Text] raises Store.Error raises Axis.Error =

    store.space.storageParent(point).let: parent =>
      dossier.content(parent.move(axis(store, name), value))

    . or(Nil)

  private def show(store: Workspace, dossier: Dossier, point: Point, mode: Mode)
  :   Optional[List[Text]] raises Store.Error raises Axis.Error =

    mode match
      case Mode.Plain  => dossier.content(point)
      case Mode.Beside => Indirect.view(dossier, point)

      case Mode.Against(name, value) =>
        Indirect.alterationView(dossier, point, former(store, dossier, point, name, value))

  // A document as the working tree shows it at a point, or `Unset` if it is not there. It is
  // not shown if a hunk stored at the point has no one place.
  def render(store: Workspace, document: Text, point: Point, mode: Mode)
  :   Optional[List[Text]] raises Store.Error raises Axis.Error =

    store.content(document, point).let: plain => show(store, store.dossier(document), point, mode)

  // A document is changed only if every hunk the store has for it, at any point, has a place.
  private def placed(store: Workspace, document: Text): Unit raises Store.Error =
    store.unplaced(List(document)).head.let(abort(_))

  // What the lines of a document in the working tree ask for.
  private def request
    ( store: Workspace, dossier: Dossier, point: Point, mode: Mode, lines: List[Text] )
  :   Indirect.Request raises Store.Error raises Axis.Error raises Indirect.Error =

    mode match
      case Mode.Plain  => Indirect.Request(Nil, lines, Nil)
      case Mode.Beside => Indirect.read(dossier, point, lines)

      case Mode.Against(name, value) =>
        Indirect.alterationRead(dossier, point, former(store, dossier, point, name, value), lines)

  private def lines(file: Text): Optional[List[Text]] raises Store.Error raises Workspace.Error =
    Disk.read(file).let(Store.lines(_))

  // How the working tree differs from what was checked out.
  def changes(store: Workspace): List[Change] raises Store.Error raises Workspace.Error =
    val present = Disk.files(store.tree)
    val known = Disk.files(pristine(store))
    val here = present.stdlib.toSet
    val there = known.stdlib.toSet

    val modified = known.filter(here.contains(_)).filter: document =>
      lines(t"${store.tree}/$document") != lines(t"${pristine(store)}/$document")

    present.filter(!there.contains(_)).map(Change(_, Kind.Added)) +
      modified.map(Change(_, Kind.Modified)) +
      known.filter(!here.contains(_)).map(Change(_, Kind.Missing))

  private def put(file: Text, content: Text, store: Workspace): Unit raises Store.Error =
    if Disk.read(file) != content then Disk.write(file, content, store.staging)

  // Makes the working tree the documents at `point`, and gives their number. Edits which are
  // not checked in prevent it, unless they are to be discarded; a file which is in the working
  // tree alone is left there.
  def checkout(store: Workspace, point: Point, force: Boolean, mode: Mode = Mode.Plain)
  :   Int raises Store.Error raises Workspace.Error raises Axis.Error =

    admit(store, point, mode)

    val contents: List[(Text, Text)] = store.documents.bind: document =>
      render(store, document, point, mode).lay(Nil): lines => List((document, Store.text(lines)))

    val written = contents.map(_(0)).stdlib.toSet
    val content = contents.stdlib.toMap

    // A file in the working tree alone is lost only if the checkout writes another over it; one
    // which already holds what would be written, as a fresh clone's tracked files do, is not.
    val pending = changes(store).filter: change =>
      change.kind != Kind.Added || content.get(change.document).exists: text =>
        Disk.read(t"${store.tree}/${change.document}") != text

    if !force && !pending.nil then abort(Workspace.Error(Workspace.Reason.Edited(pending.size)))

    Disk.files(pristine(store)).each: document =>
      if !written.contains(document) then
        Disk.remove(t"${store.tree}/$document", store.tree)
        Disk.remove(t"${pristine(store)}/$document", pristine(store))

    contents.each: (document, content) =>
      put(t"${store.tree}/$document", content, store)
      put(t"${pristine(store)}/$document", content, store)

    Disk.directory(store.tree)
    record(store, point, mode)
    contents.size

  private def refused
    ( store: Workspace, document: Text, place: Point, error: fulminate.Error, bolt: fulminate.Bolt )
  :   Workspace.Error =

    val detail = Orthant.prose(error)
    Workspace.Error(Workspace.Reason.Refused(document, store.name(place), detail, bolt))

  // The dossier of a document after its lines in the working tree are checked in.
  private def revise
    ( store:    Workspace,
      document: Text,
      point:    Point,
      mode:     Mode,
      reach:    Reach,
      content:  Optional[List[Text]] )
  :   Dossier raises Store.Error raises Workspace.Error raises Axis.Error =

    mitigate:
      case error: Checkin.Error  => refused(store, document, error.point, error, error.bolt)
      case error: Indirect.Error => refused(store, document, point, error, error.bolt)

    . protect:
        placed(store, document)
        val dossier = store.dossier(document)
        val asked = content.let(request(store, dossier, point, mode, _))
        val base = asked.lay(dossier)(Indirect.confirm(dossier, point, _))
        val lines = asked.let(_.lines)

        val stored = reach match
          case Reach.Below        => base.checkin(point, lines)

          case Reach.Since(value) =>
            Retro.since(base, point, lines, store.locate(point, List(value)))

          case Reach.Origin(name) => Retro.origin(base, point, lines, axis(store, name))

          case Reach.Only(name) =>
            val found = axis(store, name)
            base.checkin(point, lines, other => other(found) == point(found))

        asked.lay(stored): request =>
          stored.affirm(point, request.spans).unite(point, request.spans)

  // Stores the edited documents, as `reach` says; by default at the point which is checked
  // out, so that every point below it follows. `paths` names documents, or directories of
  // them, and none means all. A missing document is deleted only if `remove` is set. Nothing
  // is written if any document is refused, or if `dry` is set.
  def checkin
    ( store:  Workspace,
      paths:  List[Text],
      remove: Boolean,
      dry:    Boolean,
      reach:  Reach = Reach.Below )
  :   Tally raises Store.Error raises Workspace.Error raises Axis.Error =

    val point = view(store)
    val shown = mode(store)
    val selected = changes(store).filter: change => chosen(change.document, paths)

    selected.each: change =>
      val document = change.document

      if render(store, document, point, shown) != lines(t"${pristine(store)}/$document")
      then abort(Workspace.Error(Workspace.Reason.Moved(document)))

      if change.kind == Kind.Added && document.ends(t".delta")
      then abort(Workspace.Error(Workspace.Reason.Suffix(document)))

    val skipped = selected.filter: change => change.kind == Kind.Missing && !remove
    val stored = selected.filter: change => change.kind != Kind.Missing || remove

    val updates: List[(Change, Dossier)] = stored.map: change =>
      val content = lines(t"${store.tree}/${change.document}")
      (change, revise(store, change.document, point, shown, reach, content))

    if !dry then
      updates.fold(store): (store, update) =>
        val (change, dossier) = update
        val copy = t"${pristine(store)}/${change.document}"
        val saved = store.save(change.document, dossier)

        // The document is written again, as the store now gives it: a view's passages may
        // have changed.
        val written = render(saved, change.document, point, shown)

        written.lay(Disk.remove(copy, pristine(store))): lines =>
          Disk.write(copy, Store.text(lines), store.staging)
          put(t"${store.tree}/${change.document}", Store.text(lines), store)

        saved

    val after =
      updates.fold(store): (store, update) => store.supposing(update(0).document, update(1))

    Tally(stored, skipped, effects(store, after, stored.map(_.document)))

  // What becomes of each point when `documents` change from what one store has to what
  // another has; the points to which nothing happens are left out.
  private def effects(before: Workspace, after: Workspace, documents: List[Text])
  :   List[Effect] raises Axis.Error =

    before.space.points.bind: point =>
      val works = documents.map: document =>
        ( before.dossier(document).content(point) != after.dossier(document).content(point),
          Survey.own(after, document, point), Survey.own(before, document, point) )

      val pending =
        works.map { (changed, now, was) => now.pending.size - was.pending.size }.fold(0)(_ + _)

      val stale =
        works.map { (changed, now, was) => now.stale.size - was.stale.size }.fold(0)(_ + _)

      val changed = works.exists(_(0))

      val effect = Effect(point, changed, pending, stale)
      if changed || pending != 0 || stale != 0 then List(effect) else Nil

  // The store as it would be if the edits in the working tree were checked in, with the
  // documents whose edits could not be. Nothing is written.
  def prospect(store: Workspace)
  :   (Workspace, List[Text]) raises Store.Error raises Workspace.Error raises Axis.Error =

    val point = view(store)
    val shown = mode(store)

    changes(store).filter(_.kind != Kind.Missing).fold((store, List[Text]())): (state, change) =>
      val (next, failed) = state
      val content = lines(t"${store.tree}/${change.document}")

      safely(revise(store, change.document, point, shown, Reach.Below, content))
      . lay((next, failed + List(change.document))): dossier =>
          (next.supposing(change.document, dossier), failed)

  // The line of a document in the working tree, counted from one, at which a hunk in place at
  // the point checked out begins, in the mode the working tree is shown in. What it is found
  // from is the same for every hunk of a document, and is worked out once for each.
  def liner(store: Workspace)
  :   ((Text, Placement) => Int) raises Store.Error raises Workspace.Error raises Axis.Error =

    val point = view(store)

    mode(store) match
      case Mode.Plain =>
        (document, placement) => placement.from + 1

      case Mode.Beside =>
        // For each document, where each hunk in place ends, and the lines which the hunks up
        // to each add to the reference's: its `!` lines, and its own.
        val sums = scala.collection.mutable.HashMap[Text, (scala.Array[Int], scala.Array[Int])]()

        def added(placement: Placement): Int =
          if placement.fresh then placement.lines.size
          else placement.hunk.former.size + placement.hunk.lines.size

        (document, placement) =>
          val (ends, totals) = sums.getOrElseUpdate(document, locally:
            val placed = store.dossier(document).rendition(point).let(_.placements).or(Nil)
            (placed.map(_.end).stdlib.toArray, placed.map(added).stdlib.toArray.scanLeft(0)(_ + _)))

          // The hunks in place are in order, so those which end before this one begins are the
          // first of them; it is among them itself if it only inserts.
          val before = orthant.Content.search(ends.length)(ends(_) > placement.start)
          val own = if placement.end <= placement.start then added(placement) else 0
          val line = placement.start + totals(before) - own + 1
          if ends.length == 0 then placement.from + 1 else line

      case Mode.Against(name, value) =>
        // For each document, the lines each passage spans, and the line it begins at.
        val passages = scala.collection.mutable.HashMap[Text, List[(Int, Int, Int)]]()

        (document, placement) =>
          val found = passages.getOrElseUpdate(document, locally:
            val dossier = store.dossier(document)

            val parts =
              Indirect.alteration(dossier, point, former(store, dossier, point, name, value))
              . or(Nil)

            parts.fold((List[(Int, Int, Int)](), 0)): (state, part) =>
              val (found, count) = state

              part match
                case passage: Indirect.Passage =>
                  val size =
                    if passage.pending then passage.replaced.size
                    else passage.former.size + passage.replaced.size + passage.lines.size

                  ((passage.start, passage.end, count + 1) :: found, count + size)

                case _ =>
                  (found, count + 1)

            . apply(0).reverse)

          val within = found.filter: (start, end, line) =>
            start <= placement.start && placement.end <= end

          within.head.let(_(2)).or(placement.from + 1)

  def line(store: Workspace, document: Text, placement: Placement)
  :   Int raises Store.Error raises Workspace.Error raises Axis.Error =

    liner(store)(document, placement)

  private def chosen(document: Text, paths: List[Text]): Boolean =
    paths.nil || paths.exists: path => document == path || document.starts(t"$path/")

  // Marks as checked every stale passage of the documents at the point which is checked out,
  // and gives each such document with the number of its passages. The documents must have no
  // edits which are not checked in.
  def confirm(store: Workspace, paths: List[Text])
  :   List[(Text, Int)] raises Store.Error raises Workspace.Error raises Axis.Error =

    val point = view(store)
    val shown = mode(store)
    val edited = changes(store).filter: change => chosen(change.document, paths)

    val found: List[(Text, Int)] = store.documents.filter(chosen(_, paths)).bind: document =>
      val stale =
        store.dossier(document).rendition(point).let(_.placements.filter(!_.fresh)).or(Nil)

      if stale.nil then Nil else List((document, stale.size))

    val blocked = edited.filter: change => found.exists(_(0) == change.document)
    if !blocked.nil then abort(Workspace.Error(Workspace.Reason.Edited(blocked.size)))

    found.each: entry => placed(store, entry(0))

    found.fold(store): (store, entry) =>
      val document = entry(0)
      val saved = store.save(document, store.dossier(document).confirm(point))

      render(saved, document, point, shown).let: lines =>
        put(t"${store.tree}/$document", Store.text(lines), store)
        put(t"${pristine(store)}/$document", Store.text(lines), store)

      saved

    found

  // Shows the working tree in another mode, and gives the number of documents written. Edits
  // which are not checked in are kept: each edited document is shown as it would be if its
  // edits were checked in, beside a pristine copy in which they are not.
  def refer(store: Workspace, next: Mode)
  :   Int raises Store.Error raises Workspace.Error raises Axis.Error =

    val point = view(store)
    val shown = mode(store)
    admit(store, point, next)
    val pending = changes(store)

    val edited: List[(Text, Optional[List[Text]])] =
      pending.filter(_.kind != Kind.Missing).map: change =>
        val content = lines(t"${store.tree}/${change.document}")
        val dossier = revise(store, change.document, point, shown, Reach.Below, content)
        (change.document, show(store, dossier, point, next))

    val missing = pending.filter(_.kind == Kind.Missing).map(_.document).stdlib.toSet
    val revised = edited.map(_(0)).stdlib.toSet

    store.documents.each: document =>
      render(store, document, point, next).let: lines =>
        put(t"${pristine(store)}/$document", Store.text(lines), store)

        if !missing.contains(document) && !revised.contains(document)
        then put(t"${store.tree}/$document", Store.text(lines), store)

    edited.each: (document, content) =>
      content.let: lines => put(t"${store.tree}/$document", Store.text(lines), store)

    record(store, point, next)
    store.documents.size

  // Brings the working tree up to the store, where the store has changed beneath it, and gives
  // the documents written. An edit which is not checked in is carried onto the document as the
  // store now has it; if any cannot be, nothing is written.
  def update(store: Workspace)
  :   List[Text] raises Store.Error raises Workspace.Error raises Axis.Error =

    val point = view(store)
    val shown = mode(store)
    val known = Disk.files(pristine(store))
    val documents = (store.documents + known).distinct

    // Each document which is behind, with what to write as its pristine copy, whether the
    // tree is to be written too, and what to write there.
    val plans: List[(Text, Optional[List[Text]], Boolean, Optional[List[Text]])] =
      documents.bind: document =>
        val fresh = render(store, document, point, shown)
        val copy = lines(t"${pristine(store)}/$document")
        val tree = lines(t"${store.tree}/$document")

        if fresh == copy then Nil
        else if tree == copy then List((document, fresh, true, fresh))
        else if tree.absent then List((document, fresh, false, Unset))
        else
          val carried: Optional[List[Text]] = fresh.let: fresh =>
            tree.let: tree =>
              copy.lay(if tree == fresh then fresh else Unset): copy =>
                Retro.carry(Retro.changes(copy, tree), copy, fresh)

          if carried.absent then abort(Workspace.Error(Workspace.Reason.Clash(document)))
          List((document, fresh, true, carried))

    plans.each: (document, fresh, write, tree) =>
      val copy = t"${pristine(store)}/$document"

      fresh.lay(Disk.remove(copy, pristine(store))): lines => put(copy, Store.text(lines), store)

      if write then
        val file = t"${store.tree}/$document"
        tree.lay(Disk.remove(file, store.tree)): lines => put(file, Store.text(lines), store)

    plans.map(_(0))

  // The edits which are not checked in: for each document, the lines which differ from its
  // pristine copy, those taken out marked `-` and those put in marked `+`, each run under the
  // number of the line it begins at.
  def differences(store: Workspace, paths: List[Text])
  :   List[(Change, List[Text])] raises Store.Error raises Workspace.Error =

    changes(store).filter { change => chosen(change.document, paths) }.map: change =>
      val old = lines(t"${pristine(store)}/${change.document}").or(Nil)
      val edited = lines(t"${store.tree}/${change.document}").or(Nil)

      // In a view, whose lines have sigils of their own, the lines are marked `<` and `>`.
      val (out, in) = if mode(store) == Mode.Plain then (t"-", t"+") else (t"<", t">")

      val runs = Region.between(old, edited).filter(_.changed).bind: region =>
        val before = old.excerpt(region.from, region.to).map: line => t"$out $line"
        val after = edited.excerpt(region.from2, region.to2).map: line => t"$in $line"
        List(t"@ ${region.from + 1}") + before + after

      (change, runs)

  // Writes every document at a point, as its plain content, beneath a directory, and gives
  // their number. If `strict` is set, nothing is written while any of them wants attention.
  def publish(store: Workspace, point: Point, directory: Text, strict: Boolean)
  :   Int raises Store.Error =

    store.publish(point, Disk.path(directory), strict)

  private def key(placement: Placement): (Point, List[Text]) =
    (placement.origin, placement.hunk.lines)

  // How many other points have, in a document, some of the work which is a point's own there.
  def reach(store: Workspace, document: Text, point: Point): Int raises Axis.Error =
    val mine = Survey.own(store, document, point)
    val stale = mine.stale.map(key).stdlib.toSet
    val pending = mine.pending.stdlib.toSet

    store.space.points.count: other =>
      other != point && Survey.above(store, other).has(point) && locally:
        val theirs = Survey.work(store, document, other)

        theirs.pending.exists(pending.contains(_)) || theirs.stale.exists: placement =>
          stale.contains(key(placement))

  // How best to look at a point's work: against the change from the value before it on an
  // axis, if at that value nothing wants doing; or else beside its storage parent.
  def approach(store: Workspace, point: Point): Mode raises Axis.Error =
    store.space.storageAxis(point).lay(Mode.Plain): storage =>
      val changes: List[Mode] =
        store.space.axes.filter(_.name != storage.name).bind: axis =>
          axis.value(point(axis)).parent.lay(Nil): parent =>
            if Survey.sum(store, point.move(axis, parent)).wanting then Nil
            else List(Mode.Against(axis.name, parent))

      changes.head.or(Mode.Beside)
