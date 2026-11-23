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

import proscenium.{List, Nil}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics

// The live tree: the documents as they are being written, at the working value, in a directory
// of their own which is edited freely and committed with everything else. Orthant never writes
// there. What the store has at the live tree's point is what the tree had when it was last
// stored, so an edit is whatever differs from that, and no copy is kept to tell one by: a
// repository which has just been cloned is as ready as any.
//
// Storing the tree is a check-in at its point like any other, so the hunks of every point
// below it follow, and a translation whose lines have changed beneath it becomes stale.
object Live:
  import Worktree.{Change, Kind, Mode, Reach, Tally}

  private def place(store: Workspace): (Text, Point) raises Workspace.Error =
    store.live.or(abort(Workspace.Error(Workspace.Reason.Lifeless)))

  // How the live tree differs from what the store has at its point; nothing, in a store which
  // has no live tree. A tree which is not there is not an empty one: every document would be
  // missing from it, and so it is refused.
  def changes(store: Workspace): List[Change] raises Store.Error raises Workspace.Error =
    store.live.lay(Nil): (directory, point) =>
      if !Disk.exists(directory) then abort(Workspace.Error(Workspace.Reason.Vacant(directory)))
      val present = Disk.files(directory)
      val here = present.stdlib.toSet
      val known = store.documents.filter(store.content(_, point).present)
      val there = known.stdlib.toSet

      val modified = known.filter(here.contains(_)).filter: document =>
        Worktree.lines(t"$directory/$document") != store.content(document, point)

      present.filter(!there.contains(_)).map(Change(_, Kind.Added)) +
        modified.map(Change(_, Kind.Modified)) +
        known.filter(!here.contains(_)).map(Change(_, Kind.Missing))

  // Stores the documents of the live tree which differ from the store's, at the tree's point.
  // `paths` names documents, or directories of them, and none means all. A missing document
  // is deleted only if `remove` is set, and nothing is written if `dry` is.
  def store(store: Workspace, paths: List[Text], remove: Boolean, dry: Boolean)
  :   Tally raises Store.Error raises Workspace.Error raises Axis.Error =

    val (directory, point) = place(store)
    val selected = changes(store).filter: change => Worktree.chosen(change.document, paths)

    selected.each: change =>
      if change.kind == Kind.Added && change.document.ends(t".delta")
      then abort(Workspace.Error(Workspace.Reason.Suffix(change.document)))

    val skipped = selected.filter: change => change.kind == Kind.Missing && !remove
    val stored = selected.filter: change => change.kind != Kind.Missing || remove

    val updates: List[(Change, Dossier)] = stored.map: change =>
      val content = Worktree.lines(t"$directory/${change.document}")
      (change, Worktree.revise(store, change.document, point, Mode.Plain, Reach.Below, content))

    val kept: Optional[Workspace] = if dry || updates.nil then Unset else
      updates.fold(store): (store, update) => store.save(update(0).document, update(1))

    val after =
      updates.fold(store): (store, update) => store.supposing(update(0).document, update(1))

    Tally(stored, skipped, Worktree.effects(store, after, stored.map(_.document)), kept)

  // The store as it would be if the live tree were stored, with the documents which could not
  // be. Nothing is written.
  def prospect(store: Workspace)
  :   (Workspace, List[Text]) raises Store.Error raises Workspace.Error raises Axis.Error =

    store.live.lay((store, List[Text]())): (directory, point) =>
      changes(store).filter(_.kind != Kind.Missing).fold((store, List[Text]())): (state, change) =>
        val (next, failed) = state
        val content = Worktree.lines(t"$directory/${change.document}")

        safely(Worktree.revise(next, change.document, point, Mode.Plain, Reach.Below, content))
        . lay((next, failed + List(change.document))): dossier =>
            (next.supposing(change.document, dossier), failed)

  // The axis and the working value which a release as `name` would be of. It is refused on an
  // axis which follows a repository, where a release is a tag of that; and a name which is not
  // valid, or which a value has already.
  def releasing(store: Workspace, name: Text)
  :   (AxisEntry, ValueEntry) raises Store.Error raises Workspace.Error =

    val (entry, value) =
      store.settings.working.or(abort(Workspace.Error(Workspace.Reason.Unworking)))

    if entry.kind == t"git" then abort(Workspace.Error(Workspace.Reason.Tagged(entry.name)))
    if !Store.valid(name) then abort(Workspace.Error(Workspace.Reason.Name(name)))

    store.owner(name).let: other =>
      abort(Workspace.Error(Workspace.Reason.ValueExists(other.name, name)))

    (entry, value)

  // The store in which the working value has been released as `name`: it takes that name,
  // with everything stored at it, and a new value of its former name, at which nothing is
  // stored, is the working value beneath it. No content changes. The configuration is written.
  def release(store: Workspace, name: Text)
  :   Workspace raises Store.Error raises Workspace.Error raises Axis.Error =

    val (entry, value) = releasing(store, name)
    val renamed = Sync.rename(store, entry.name, value.name, name, _.copy(working = Unset))
    Sync.add(renamed, entry.name, ValueEntry(value.name, name, working = true)).inscribe()

  // The store whose live tree is the directory given, relative to the store's parent. What
  // is there is not read: whatever differs from the store's documents is an edit from then on.
  def settle(store: Workspace, directory: Text): Workspace raises Store.Error =
    store.configure(store.settings.copy(live = directory))

  // The store in which a value is the working value, and no other is.
  def working(store: Workspace, axis: Text, value: Text)
  :   Workspace raises Store.Error raises Workspace.Error =

    val unknown = Store.Error(Store.Reason.Unknown(axis))
    val found = store.entry(axis).or(abort(unknown))
    if found.source.present then abort(Workspace.Error(Workspace.Reason.Sourced(axis)))
    val name = store.named(axis, value).or(abort(Store.Error(Store.Reason.Unknown(value))))

    val entries = store.settings.axis.map: entry =>
      entry.copy(value = entry.value.map: other =>
        other.copy(working = if entry.name == axis && other.name == name then true else Unset))

    store.configure(store.settings.copy(axis = entries))
