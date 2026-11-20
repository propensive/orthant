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
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.filesystemBackends.javaBaseFilesystem
import soundness.systems.javaBaseSystem

// Times the reading of a store on disk, the derivation of every document at every point, and
// the pending lines of each: `java -cp <test jar> orthant.timing <store's home>`.
@main
def timing(root: String): Unit =
  import strategies.throwUnsafely
  def now: Long = java.lang.System.nanoTime
  def since(start: Long): String = ((now - start)/1000000L).toString+"ms"
  def say(text: String): Unit = java.lang.System.out.nn.println(text)
  val start = now
  val store = Store.inspect(Disk.path(root.tt))
  say("read "+store.documents.size.toString+" documents in "+since(start))

  store.points.each: point =>
    val begin = now

    val lines = store.documents.map: document =>
      store.dossier(document).content(point).let(_.size).or(0)

    val issues = store.documents.map { document => store.dossier(document).issues(point).size }
    val pending = store.documents.map { document => store.dossier(document).pending(point).size }

    say:
      store.name(point).s+": "+lines.fold(0)(_ + _).toString+" lines, "+issues.fold(0)(_ + _).toString+
        " issues, "+pending.fold(0)(_ + _).toString+" pending in "+since(begin)

  val check = now
  say("faults: "+store.faults.size.toString+" in "+since(check))
  say("total "+since(start))
