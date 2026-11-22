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

import proscenium.List
import soundness.*

// The repository whose commits the values of a sourced axis stand for, as a reader needs it:
// beside the store's home, which is `Perusable`, it is what a store with a sourced axis is read
// from. A commit is named by its hash, in full, and what a commit has never changes.
trait Upstream:
  // How the repository is named when an error is reported.
  def name: Text

  // The bytes of the file at a path in a commit's tree, if there is a regular file there.
  def blob(commit: Text, path: Text): Optional[Data] raises Store.Error

  // Whether `upper` is `lower`, or an ancestor of it.
  def ancestor(upper: Text, lower: Text): Boolean raises Store.Error

  // The entries of a directory of a commit's tree, in any order; none if it does not exist.
  def entries(commit: Text, directory: Text): List[Perusable.Entry] raises Store.Error
