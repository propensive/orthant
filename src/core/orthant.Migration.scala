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
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.systems.javaBaseSystem

// The making of a store of format 1 into one of this format. In that format the records of a
// point were in one directory of `.orthant/data`, named `root` for the root point, and
// otherwise for each axis on which the point is not at the root, as `axis=value` pairs joined
// by `,`, in any order; two axes could each have a value of one name; and a delta had every
// line of each hunk's anchor, where it now has their imprint.
object Migration:
  // The point which a directory of a store of format 1 names.
  private def point(space: Space, name: Text): Point raises Store.Error =
    def invalid: Nothing = abort(Store.Error(Store.Reason.Folder(name)))

    if name == t"root" then space.root else name.cut(t",").fold(space.root): (point, pair) =>
      pair.cut(t"=") match
        case axis :: value :: Nil =>
          val found = space.axes.filter(_.name == axis).head.or(invalid)
          val value2 = Store.unfolded(value)
          if !found.has(value2) || value2 == found.root then invalid
          if point(found) != found.root then invalid
          point.move(found, value2)

        case _ =>
          invalid

  // Makes the store beneath `root` one of this format, and gives the number of points whose
  // records were moved; or nothing, if it is one already. Nothing is moved unless everything
  // can be, and the configuration is written last: a store which was left part moved is still
  // of format 1, and is made whole by migrating it again, since no directory of this format has
  // a name which one of that format could have.
  def apply(root: Text): Optional[Int] raises Store.Error raises Axis.Error =
    val former = Store.written(Disk.path(root))

    if former.format == Settings.format then Unset else
      if former.format != Settings.legacy then
        val (found, legacy) = (former.format, Settings.legacy)
        val detail = t"the store is of format $found, and not $legacy"
        abort(Store.Error(Store.Reason.Config(detail)))

      val settings = Store.checked(former.copy(format = Settings.format))
      val space = settings.space
      val store = Workspace(root, new Store(settings, space, Map()))

      Disk.locked(store.lock):
        val moves = Disk.directories(store.data).bind: name =>
          val source = t"${store.data}/$name"

          if name == t"root" || name.contains(t"=") then
            val target = t"${store.data}/${store.name(point(space, name))}"

            if Disk.exists(target)
            then abort(Store.Error(Store.Reason.Access(target, t"it is already there")))

            List((source, target))
          else if name == Store.records || store.owner(Store.unfolded(name)).present then
            Nil
          else
            abort(Store.Error(Store.Reason.Folder(name)))

        moves.each: (source, target) => Disk.move(source, target)

        // Each delta is written again as it is now written: with the imprint of each hunk's
        // anchor where its lines were. One which cannot be read as a delta of the version
        // before is left as it is, to be found when the store is read; and in the root
        // point's records a file is no delta.
        Disk.files(store.data).each: file =>
          if file.ends(t".delta") && !file.starts(t"${Store.records}/") then
            val path = t"${store.data}/$file"

            Disk.read(path).let: text =>
              safely(Delta.upgrade(Store.lines(text))).let: record =>
                Disk.write(path, Store.text(Delta.serialize(record)), store.staging)

        store.inscribe()
        moves.size
