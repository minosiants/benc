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

import scodec.bits.BitVector

trait BTypeSyntax {
  implicit def btypeSyntax(bt: BType): BTypeOps = new BTypeOps(bt)
}

/**
  * Some Ops for `BType`
  * @param bt
  */
final class BTypeOps(val bt: BType) extends AnyVal {

  /**
    * Converts this `BType` value to bencoding binary
    */
  def toBenc: Result[BitVector] = ToBenc.instance.toBenc(bt)

  /**
    * Converts this `BType` value to A representation
    */
  def as[A: BDecoder]: Result[A] = BDecoder[A].decode(bt)

  def get[A: BDecoder](name: String): Result[A] =
    BDecoder.at[A](name).decode(bt)

  def down(name: String): Result[BType] = BDecoder.down(name).decode(bt)

  def -(name: String): Result[BType] =
    bt.remove(name).toRight(BencError.CodecError(s"field '$name' is not found"))
}
