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

import BType._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._
import cats.syntax.either._
import scodec.codecs._
import scodec.bits.BitVector
import shapeless._
import shapeless.labelled.FieldType

trait BEncoder[A] { self =>

  def encode(a: A): Result[BType]

  def contramap[B](f: B => A): BEncoder[B] =
    BEncoder.instance(v => encode(f(v)))

  def econtramap[B](f: B => Result[A]): BEncoder[B] =
    BEncoder.instance(v => f(v).flatMap(encode))
}

object BEncoder {
  def apply[A](implicit F: BEncoder[A]): BEncoder[A] = F

  def instance[A](f: A => Result[BType]): BEncoder[A] = new BEncoder[A] {
    override def encode(a: A): Result[BType] = f(a)
  }

  trait BMapEncoder[A] extends BEncoder[A] {
    def encode(a: A): Result[BMap]
  }

  def bmapInstance[A](f: A => Result[BMap]): BMapEncoder[A] =
    new BMapEncoder[A] {
      override def encode(a: A): Result[BMap] = f(a)
    }

  implicit val bitVectorBEncoder: BEncoder[BitVector] = instance(
    bits => BString(bits).asRight
  )
  implicit val stringBEncoder: BEncoder[String] = bitVectorBEncoder.econtramap(
    str =>
      utf8.encode(str).toEither.leftMap(err => Error.CodecError(err.message))
  )
  implicit val longBEncoder: BEncoder[Long] = instance(num => BNum(num).asRight)
  implicit val intBEncoder: BEncoder[Int]   = longBEncoder.contramap(_.toLong)
  implicit def listBEncoder[A: BEncoder]: BEncoder[List[A]] =
    instance(_.traverse(v => BEncoder[A].encode(v)).map(BList))

  //To handle option case in hlistEncoder
  trait OptionBEncoder[A] extends BEncoder[A]

  implicit def optionBencoder[A: BEncoder]: BEncoder[Option[A]] =
    new OptionBEncoder[Option[A]] {
      override def encode(a: Option[A]): Result[BType] = a match {
        case Some(value) => BEncoder[A].encode(value)
        case None        => Error.NotFound.asLeft
      }
    }

  implicit val hnilEncoder: BMapEncoder[HNil] = bmapInstance(
    _ => BMap.empty.asRight
  )

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
      implicit
      witness: Witness.Aux[K],
      henc: Lazy[BEncoder[H]],
      tenc: BMapEncoder[T]
  ): BMapEncoder[FieldType[K, H] :: T] = {
    val name = witness.value.name
    bmapInstance { v =>
      val value: Result[Map[String, BType]] =
        (henc.value.encode(v.head), henc.value) match {
          case (Right(h), _) => Map(name -> h).asRight
          case (Left(Error.NotFound), _: OptionBEncoder[_]) =>
            Map.empty[String, BType].asRight
          case (Left(err), _) => err.asLeft
        }
      for {
        head <- value
        tail <- tenc.encode(v.tail)
      } yield BMap(head ++ tail.m)
    }
  }

  implicit def genericEncoder[A, H](
      implicit
      gen: LabelledGeneric.Aux[A, H],
      hencoder: Lazy[BMapEncoder[H]]
  ): BEncoder[A] =
    bmapInstance { v =>
      hencoder.value.encode(gen.to(v))
    }

}
