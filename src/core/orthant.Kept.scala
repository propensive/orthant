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

import soundness.*

// The stores which this process has read, each as it was when it was last read. A command in
// a daemon which has read a store before is given it again if no file of it has changed, and
// with it everything which was derived in it; if one has, the store is read again, and takes
// from the one before the dossier of each document whose records are the same. A store is
// kept only for as long as the memory it takes is not wanted for anything else.
object Kept:
  private case class Entry(stamps: Disk.Stamps, store: Workspace)

  private val entries =
    scala.collection.concurrent.TrieMap[Text, java.lang.ref.SoftReference[Entry]]()

  private def stamps(root: Text): Disk.Stamps raises Store.Error =
    Disk.stamps(t"$root/.orthant/config.tel", t"$root/.orthant/data")

  private def kept(root: Text): Optional[Entry] =
    entries.get(root).map { reference => Optional(reference.get) }.getOrElse(Unset)

  // The store beneath `root`, of which every record must be readable, as `Workspace.open`
  // gives it. The files are stamped before any is read, so one which changes while they are
  // read is read again by whatever opens the store next.
  def open(root: Text): Workspace raises Store.Error =
    val former = kept(root)

    former.let { entry => if Disk.unchanged(entry.stamps) then entry.store else Unset }.or:
      val found = stamps(root)
      val read = Workspace.open(root)
      val store = former.lay(read) { entry => read.sharing(entry.store) }
      entries(root) = java.lang.ref.SoftReference(Entry(found, store))
      store

  // Keeps a store which has just been written, as what its files now hold.
  def hold(store: Workspace): Unit raises Store.Error =
    entries(store.root) = java.lang.ref.SoftReference(Entry(stamps(store.root), store))
