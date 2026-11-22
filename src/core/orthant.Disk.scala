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
import soundness.charsets.utf8Charset
import soundness.codepages.utf8Codepage
import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.filesystemOptions.createParents
import soundness.filesystemOptions.deleteRecursively
import soundness.filesystemOptions.dereferenceSymlinks
import soundness.filesystemOptions.moveAtomically
import soundness.filesystemOptions.overwritePreexisting
import soundness.pathInterfaces.pathOnLocal
import soundness.sortingAlgorithms.timsort
import soundness.systems.javaBaseSystem
import soundness.textSanitizers.skipSanitizer

// The few things Orthant does to a disk, each naming a file by its absolute path as text, and
// each failing with a `Store.Error` which names the file.
object Disk:
  private type Raising[result] =
    (Tactic[Io.Error], Tactic[Path.Error], Tactic[Truncation.Error]) ?=> result

  private def access[result](file: Text)(block: Raising[result]): result raises Store.Error =
    def failed(error: Error): Store.Error =
      Store.Error(Store.Reason.Access(file, Orthant.prose(error)))

    mitigate:
      case error: Io.Error         => failed(error)
      case error: Path.Error       => failed(error)
      case error: Truncation.Error => failed(error)

    . protect(block)

  // The directory which a path names, as the reader takes a store's home.
  def path(directory: Text): Path on Local raises Store.Error =
    access(directory)(directory.as[Path on Local])

  // The directory a file is in.
  def parent(file: Text): Text =
    val parts = file.cut(t"/")
    parts.excerpt(0, parts.count { part => true } - 1).join(t"/")

  def exists(file: Text): Boolean raises Store.Error =
    access(file)(file.as[Path on Local].existent())

  def read(file: Text): Optional[Text] raises Store.Error = access(file):
    val path = file.as[Path on Local]
    if path.existent() then path.read[Text] else Unset

  def directory(file: Text): Unit raises Store.Error = access(file):
    val path = file.as[Path on Local]
    if !path.existent() then path.create[Directory](CreateFlag.Parents)

  // The file is written beside the store and moved into place, so that no reader sees part of it.
  def write(file: Text, content: Text, staging: Text): Unit raises Store.Error = access(file):
    directory(parent(staging))
    val temporary = staging.as[Path on Local]
    temporary.write(content)
    temporary.moveTo(file.as[Path on Local])

  // Moves a file or a directory, into a directory which is made if it is not there.
  def move(source: Text, target: Text): Unit raises Store.Error = access(source):
    directory(parent(target))
    source.as[Path on Local].moveTo(target.as[Path on Local])

  // Deletes a file, and each directory this leaves empty, as far as `within`.
  def remove(file: Text, within: Text): Unit raises Store.Error = access(file):
    val path = file.as[Path on Local]
    if path.existent() then path.delete()

    def prune(directory: Text): Unit =
      if directory != within && directory.starts(within) then
        val path = directory.as[Path on Local]

        if path.existent() && path.children.to[List].nil then
          path.delete()
          prune(parent(directory))

    prune(parent(file))

  private def hidden(name: Text): Boolean = name.starts(t".")

  // The names of the directories in a directory, in order; none if it does not exist.
  def directories(directory: Text): List[Text] raises Store.Error = access(directory):
    val path = directory.as[Path on Local]

    if !path.existent() then Nil else
      val names = path.children.to[List].bind: child =>
        child.entry() match
          case Directory => List(child.name)
          case _         => Nil

      names.filter(!hidden(_)).order: name => name

  // The files beneath a directory, as paths relative to it, in order; none if it does not
  // exist. Hidden files and directories are passed over.
  def files(directory: Text): List[Text] raises Store.Error = access(directory):
    def recur(path: Path on Local, prefix: Text): List[Text] =
      path.children.to[List].bind: child =>
        if hidden(child.name) then Nil else child.entry() match
          case Directory => recur(child, t"$prefix${child.name}/")
          case File      => List(t"$prefix${child.name}")
          case _         => Nil

    val path = directory.as[Path on Local]
    if !path.existent() then Nil else recur(path, t"").order: name => name

  // Runs the block while holding the lock file, which keeps other invocations out.
  def locked[result](file: Text)(block: => result): result raises Store.Error =
    directory(parent(file))

    access(file):
      val path = file.as[Path on Local]
      path.open[File](Read & Write & Exclusive, OpenFlag.Create): handle ?=> block
