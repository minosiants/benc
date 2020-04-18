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

import scala.Function._
import scala.collection.immutable.ListMap

abstract sealed class BType extends Product with Serializable {

  def fold[A](
      bstring: BitVector => A,
      bnum: Long => A,
      blist: List[BType] => A,
      bmap: Map[String, BType] => A
  ): A = this match {
    case BType.BString(bits) => bstring(bits)
    case BType.BNum(value)   => bnum(value)
    case BType.BList(list)   => blist(list)
    case BType.BMap(map)     => bmap(map)
  }
  def bstring: Option[BitVector] =
    fold(Some(_), const(None), const(None), const(None))

  def bnum: Option[Long] = fold(const(None), Some(_), const(None), const(None))

  def blist: Option[List[BType]] =
    fold(const(None), const(None), Some(_), const(None))

  def bmap: Option[Map[String, BType]] =
    fold(const(None), const(None), const(None), Some(_))

}

object BType {
  final case class BString(bits: BitVector)    extends BType
  final case class BNum(value: Long)           extends BType
  final case class BList(list: List[BType])    extends BType
  final case class BMap(m: Map[String, BType]) extends BType

  object BMap {
    val empty = BMap(ListMap.empty[String, BType])
  }

}
