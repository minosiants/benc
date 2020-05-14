/*
 * Copyright 2020 Kaspar Minosiants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package benc

import cats.{ Eq, Show }

import scala.util.control.NoStackTrace

/**
  * Library error representation
  */
sealed abstract class BencError
    extends NoStackTrace
    with Product
    with Serializable

object BencError {
  final case object NotFound               extends BencError
  final case class CodecError(msg: String) extends BencError

  implicit val showBenError: Show[BencError] = Show.show {
    case NotFound        => "Not found"
    case CodecError(msg) => s"Codec error: $msg"
  }
  implicit val eqBencError: Eq[BencError] = Eq.fromUniversalEquals
}
