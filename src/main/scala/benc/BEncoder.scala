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
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Keys

/**
  * Supports encoding a value of type `A` to BType.
  * @tparam A - Tyoe to be encoded
  */
trait BEncoder[A] { self =>

  /**
    * Attempts to encode a value of type `A`  to `BType`
    * @param a - to encode
    * @return Error if can not to encode or encoded result `BType`
    */
  def encode(a: A): Result[BType]

  /**
    * Converts this encoder to an `BEncoder[B]` using the supplied `B => A`.
    */
  def contramap[B](f: B => A): BEncoder[B] =
    BEncoder.instance(v => encode(f(v)))

  /**
    * Converts this encoder to an `BEncoder[B]` using the supplied `B => Result[A]`.
    */
  def econtramap[B](f: B => Result[A]): BEncoder[B] =
    BEncoder.instance(v => f(v).flatMap(encode))
}

/**
  * Companion for [[BEncoder]]
  */
object BEncoder {

  /**
    * Creates `BEncoder[A]`  using implicit value
    */
  def apply[A](implicit F: BEncoder[A]): BEncoder[A] = F

  /**
    * Constructor.
    * Creates `BEncoder[A]` using provided f: BType => Result[BType].
    */
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

  /**
    * BEncoder[BitVector]
    */
  implicit val bitVectorBEncoder: BEncoder[BitVector] = instance(
    bits => BString(bits).asRight
  )

  /**
    * BEncoder[String]
    */
  implicit val stringBEncoder: BEncoder[String] = bitVectorBEncoder.econtramap(
    str =>
      utf8
        .encode(str)
        .toEither
        .leftMap(err => BencError.CodecError(err.message))
  )

  /**
    * BEncoder[Long]
    */
  implicit val longBEncoder: BEncoder[Long] = instance(num => BNum(num).asRight)

  /**
    * BEncoder[Int]
    */
  implicit val intBEncoder: BEncoder[Int] = longBEncoder.contramap(_.toLong)

  /**
    * BEncoder[List[A]]
    * @tparam A - encoder for list elements
    */
  implicit def listBEncoder[A: BEncoder]: BEncoder[List[A]] =
    instance(_.traverse(v => BEncoder[A].encode(v)).map(BList))

  //To handle option case in hlistEncoder
  trait OptionBEncoder[A] extends BEncoder[A]

  /**
    *  BEncoder[Option[A]]
    * @tparam A encoder for option element
    */
  implicit def optionBEncoder[A: BEncoder]: BEncoder[Option[A]] =
    new OptionBEncoder[Option[A]] {
      override def encode(a: Option[A]): Result[BType] = a match {
        case Some(value) => BEncoder[A].encode(value)
        case None        => BencError.NotFound.asLeft
      }
    }

  trait HListBEncoder[L <: HList] {
    def encode(ann: Map[String, String]): BMapEncoder[L]
  }
  object HListBEncoder {

    def instance[L <: HList](
        f: Map[String, String] => BMapEncoder[L]
    ): HListBEncoder[L] = new HListBEncoder[L] {
      override def encode(ann: Map[String, String]): BMapEncoder[L] = f(ann)
    }

  }

  implicit val hnilEncoder: HListBEncoder[HNil] = HListBEncoder.instance { _ =>
    bmapInstance(
      _ => BMap.empty.asRight
    )
  }

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
      implicit
      fieldName: FieldName,
      witness: Witness.Aux[K],
      henc: Lazy[BEncoder[H]],
      tenc: HListBEncoder[T]
  ): HListBEncoder[FieldType[K, H] :: T] = {
    HListBEncoder.instance { ann =>
      val name =
        ann.getOrElse(witness.value.name, fieldName.name(witness.value))
      bmapInstance { v =>
        val value: Result[Map[String, BType]] =
          (henc.value.encode(v.head), henc.value) match {
            case (Right(h), _) => Map(name -> h).asRight
            case (Left(BencError.NotFound), _: OptionBEncoder[_]) =>
              Map.empty[String, BType].asRight
            case (Left(err), _) => err.asLeft
          }
        for {
          head <- value
          tail <- tenc.encode(ann).encode(v.tail)
        } yield BMap(head ++ tail.m)
      }
    }
  }

  implicit def genericEncoder[
      A,
      R <: HList,
      D <: HList,
      F <: HList,
      K <: HList
  ](
      implicit
      gen: LabelledGeneric.Aux[A, R],
      underlying: Lazy[HListBEncoder[R]],
      fields: Keys.Aux[R, F],
      fieldsToList: ToTraversable.Aux[F, List, Symbol],
      keys: Annotations.Aux[BencKey, A, K],
      keysToList: ToTraversable.Aux[K, List, Option[BencKey]]
  ): BMapEncoder[A] = {
    bmapInstance { v =>
      val keyAnnotationMap: Map[String, String] =
        fieldsToList(fields())
          .map(_.name)
          .zip(keysToList(keys()))
          .collect {
            case (field, Some(keyAnnotation)) => (field, keyAnnotation.value)
          }
          .toMap

      underlying.value.encode(keyAnnotationMap).encode(gen.to(v))
    }
  }

}
