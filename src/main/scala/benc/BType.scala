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

/**
  *  A data type representing possible <a href="https://wiki.theory.org/index.php/BitTorrentSpecification#Bencoding">Bencoding</a> values.
  */
abstract sealed class BType extends Product with Serializable {

  /**
    * The catamorphism for the JSON value data type.
    */
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

  /**
    *  Returns the possible BitVector of this BType value.
    */
  def bstring: Option[BitVector] =
    fold(Some(_), const(None), const(None), const(None))

  /**
    *  Returns the possible Long of this BType value.
    */
  def bnum: Option[Long] = fold(const(None), Some(_), const(None), const(None))

  /**
    *  Returns the possible List of BType of this BType value.
    */
  def blist: Option[List[BType]] =
    fold(const(None), const(None), Some(_), const(None))

  /**
    *  Returns the possible Map of this BType value.
    */
  def bmap: Option[Map[String, BType]] =
    fold(const(None), const(None), const(None), Some(_))

  def rbmap: Result[Map[String, BType]] =
    bmap.toRight(BencError.CodecError(s"Not bmap: ${this}"))

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
