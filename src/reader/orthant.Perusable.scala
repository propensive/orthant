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
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemOptions.overwritePreexisting
import soundness.filesystemOptions.preserveSymlinks

// Where a store is read from. A store is defined over a tree of directories with named entries,
// and files with bytes (spec/store.md §5), and a type which presents one is `Perusable`: a
// directory of a filesystem is, and so might be a tree of a git repository, or a map held in
// memory. Its value is a store's home: the directory which has `.orthant` in it.
//
// A directory or a file is named by its path beneath the home, as components joined by `/`.
object Perusable:
  enum Kind:
    case Directory, File, Other

  // An entry of a directory. One which is neither a directory nor a regular file — a symbolic
  // link, say — is `Other`, and is not followed.
  case class Entry(name: Text, kind: Kind)

  private type Raising[result] = (Tactic[Io.Error], Tactic[Path.Error]) ?=> result

  private[orthant] def access[result](file: Text)(block: Raising[result])
  :   result raises Store.Error =

    def failed(error: Error): Store.Error =
      Store.Error(Store.Reason.Access(file, Orthant.prose(error)))

    mitigate:
      case error: Io.Error   => failed(error)
      case error: Path.Error => failed(error)

    . protect(block)

  private[orthant] def descend[plane <: Platform](home: Path on plane, path: Text): Path on plane =
    path.cut(t"/").filter(_ != t"").fold(home): (path, name) => path.unsafeChild(name)(using Unsafe)

  given filesystem: [plane <: Platform: {Filesystem, Explorable}, path <: Path on plane]
  =>  ( FilesystemBackend on plane )
  =>  path is Perusable =

    new Perusable:
      type Self = path

      def name(home: path): Text = home.encode

      def entries(home: path, directory: Text): List[Entry] raises Store.Error =
        access(t"${name(home)}/$directory"):
          val found: Path on plane = descend[plane](home, directory)

          if !found.existent() then Nil else found.children.to[List].map: child =>
            child.entry() match
              case Directory => Entry(child.name, Kind.Directory)
              case File      => Entry(child.name, Kind.File)
              case _         => Entry(child.name, Kind.Other)

      def data(home: path, file: Text): Optional[Data] raises Store.Error =
        access(t"${name(home)}/$file"):
          val found: Path on plane = descend[plane](home, file)
          if found.existent() then found.read[Data] else Unset

trait Perusable extends Typeclass:
  // How the home is named when an error is reported.
  def name(home: Self): Text

  // The entries of a directory beneath the home, in any order; none if it does not exist.
  def entries(home: Self, directory: Text): List[Perusable.Entry] raises Store.Error

  // The bytes of a file beneath the home, if it exists.
  def data(home: Self, file: Text): Optional[Data] raises Store.Error

// A place from which a store may be looked for: one which has places above it, each of which
// might be a store's home.
object Ascendable:
  given filesystem: [plane <: Platform, path <: Path on plane]
  =>  path is Ascendable to (Path on plane) =

    new Ascendable:
      type Self = path
      type Result = Path on plane

      def ancestors(directory: path): List[Path on plane] =
        def recur(current: Path on plane): List[Path on plane] =
          current :: current.parent.lay(Nil)(recur(_))

        recur(directory)

trait Ascendable extends Typeclass, Resultant:
  // The place itself, and then each place above it, from the nearest.
  def ancestors(directory: Self): List[Result]

// A directory into which documents may be written, as an export writes them. It is never part
// of a store.
object Depositable:
  given filesystem: [plane <: Platform: Filesystem, path <: Path on plane]
  =>  ( FilesystemBackend on plane )
  =>  path is Depositable =

    new Depositable:
      type Self = path

      def deposit(directory: path, file: Text, data: Data): Unit raises Store.Error =
        Perusable.access(t"${directory.encode}/$file"):
          val parts = file.cut(t"/")
          val above = parts.excerpt(0, parts.count { part => true } - 1).join(t"/")
          val folder: Path on plane = Perusable.descend[plane](directory, above)
          if !folder.existent() then folder.create[Directory](CreateFlag.Parents)
          Perusable.descend[plane](directory, file).write(data)

trait Depositable extends Typeclass:
  // Writes a file beneath the directory, and whatever directories it is in.
  def deposit(directory: Self, file: Text, data: Data): Unit raises Store.Error
