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

// What is common to every part of Orthant. Every error Orthant raises is named by a bolt code
// whose project letter is `o`; the subdivisions are tabulated in `design/engine.md`:
//
//   1  the store format      4  the working tree and its views
//   2  axes and points       5  the configuration
//   3  check-in              6  the command line
//                            7  synchronizing an axis with git
object Orthant:
  // The letter by which bolt codes name Orthant.
  final val letter = 'o'

  def bolt(subdivision: Int, error: Int, variant: Int = 0): fulminate.Bolt =
    fulminate.Bolt(letter, subdivision, error, variant)

  // What an error says, as one line: a message laid out over several lines begins and ends
  // with a line break, which is no part of it.
  def prose(error: fulminate.Error): Text = error.message.text.trim
