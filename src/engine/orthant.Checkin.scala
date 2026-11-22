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

// Why a check-in was refused. Nothing is merged interactively: what does not commute is refused.
object Checkin:
  enum Reason:
    case Unequal, Scope, Unplaced, Carry, Unreached, Sourced

  case class Error(point: Point, reason: Reason)(using Diagnostics)
  extends fulminate.Error
    ( reason match
        case Reason.Unequal  => m"the checked-in content could not be stored exactly"
        case Reason.Scope    => m"the check-in would change a point outside its scope"
        case Reason.Unplaced => m"the check-in would leave a stored hunk without its lines"
        case Reason.Carry    => m"a change could not be carried there: the lines differ"

        case Reason.Unreached =>
          m"the changes, stored where they were sent, would not give the checked-in content"

        case Reason.Sourced =>
          m"the content there is the source's, and nothing is stored over it" ):

    override def bolt: fulminate.Bolt = reason match
      case Reason.Unequal   => Orthant.bolt(3, 1)
      case Reason.Scope     => Orthant.bolt(3, 2)
      case Reason.Unplaced  => Orthant.bolt(3, 3)
      case Reason.Carry     => Orthant.bolt(3, 4)
      case Reason.Unreached => Orthant.bolt(3, 5)
      case Reason.Sourced   => Orthant.bolt(3, 6)
