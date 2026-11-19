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

// A store held in memory, as the bytes of each of its files by the file's path beneath its
// home: a home which is no directory of any filesystem, by which the reader is shown to need
// none.
object Archive:
  given perusable: Archive is Perusable = new Perusable:
    type Self = Archive

    def name(home: Archive): Text = t"memory"

    def entries(home: Archive, directory: Text): List[Perusable.Entry] raises Store.Error =
      val prefix = t"$directory/"

      val found = home.files.map(_(0)).filter(_.starts(prefix)).map: file =>
        file.skip(prefix.length).cut(t"/") match
          case name :: Nil => Perusable.Entry(name, Perusable.Kind.File)
          case name :: _   => Perusable.Entry(name, Perusable.Kind.Directory)
          case _           => Perusable.Entry(t"", Perusable.Kind.Other)

      found.fold(List[Perusable.Entry]()): (seen, entry) =>
        if seen.has(entry) then seen else entry :: seen

    def data(home: Archive, file: Text): Optional[Data] raises Store.Error =
      home.files.filter(_(0) == file).head.let(_(1))

case class Archive(files: List[(Text, Data)])
