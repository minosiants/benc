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

import cats.Monoid
import scodec.bits.BitVector

import scala.Function._
import scala.collection.immutable.ListMap

/**
  *  A data type representing possible <a href="https://wiki.theory.org/index.php/BitTorrentSpecification#Bencoding">Bencoding</a> values.
  */
abstract sealed class BType extends Product with Serializable {
  import BType._

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


  def field(name: => String): Option[BType] = bmap.flatMap(_.get(name))
  
  def combine(bt: BType): BType = {
    (this, bt) match {
      case (a @ BString(_), b @ BString(_)) => bstringMonoid.combine(a, b)
      case (a @ BNum(_), b @ BNum(_))       => bnumMonoid.combine(a, b)
      case (a @ BList(_), b @ BList(_))     => blistMonoid.combine(a, b)
      case (a @ BMap(_), b @ BMap(_))       => bmapMonoid.combine(a, b)
      case (_, _)                           => this
    }
  }
}

object BType {
  final case class BString(bits: BitVector)    extends BType
  final case class BNum(value: Long)           extends BType
  final case class BList(list: List[BType])    extends BType
  final case class BMap(m: Map[String, BType]) extends BType

  implicit val bstringMonoid: Monoid[BString] =
    Monoid.instance(
      BString(BitVector.empty),
      (a, b) => BString(a.bits ++ b.bits)
    )

  implicit val bnumMonoid: Monoid[BNum] =
    Monoid.instance(BNum(0), (a, b) => BNum(a.value + b.value))

  implicit val blistMonoid: Monoid[BList] =
    Monoid.instance(BList(List.empty), (a, b) => BList(a.list ++ b.list))

  implicit val bmapMonoid: Monoid[BMap] = Monoid.instance(
    BMap(ListMap.empty[String, BType]),
    (a, b) => BMap(a.m ++ b.m)
  )

  val emptyBString: BType = bstringMonoid.empty

  val emptyBMap: BType = bmapMonoid.empty

  def singleBMap(field: String, value: BType): BType =
    BMap(ListMap(field -> value))

}
