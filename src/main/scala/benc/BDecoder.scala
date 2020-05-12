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

import benc.BType._
import cats.Monad
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.traverse._
import scodec.bits.BitVector
import scodec.codecs._
import shapeless._
import shapeless.labelled.{ FieldType, field }
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Keys

/**
  * Supports decoding from `BType` to a value of type `A`.
  *
  * @tparam A - type to which decoding is done
  */
trait BDecoder[A] {

  /**
    * Attempts to decode from `BType` to a value of type `A`.
    * @param bt to decode
    * @return error if can not be decoded or decoded `A`.
    */
  def decode(bt: BType): Result[A]

  /**
    * Converts this decoder to a `BDecoder[B]` using the supplied `A => B`.
    */
  def map[B](f: A => B): BDecoder[B] = BDecoder.instance { bt =>
    decode(bt).map(f)
  }

  /**
    * Converts this decoder to a `BDecoder[B]` using the supplied `A => BDecoder[B]`.
    * @group combinators
    */
  def flatMap[B](f: A => BDecoder[B]): BDecoder[B] =
    BDecoder.instance { bt =>
      decode(bt).flatMap(f(_).decode(bt))
    }

  /**
    * Converts this decoder to a `BDecoder[B]` using the supplied `A => Result[B]`.
    */
  def emap[B](f: A => Result[B]): BDecoder[B] = BDecoder.instance(
    bt => decode(bt) flatMap (f(_))
  )

}

/**
  * Companion for [[BDecoder]].
  */
object BDecoder {

  /**
    * Creates `BDecoder[A]`  using implicit value
    */
  final def apply[A](implicit F: BDecoder[A]): BDecoder[A] = F

  /**
    * Constructor.
    * Creates `BDecoder[A]` using provided f: BType => Result[A].
    */
  def instance[A](f: BType => Result[A]): BDecoder[A] =
    new BDecoder[A] {
      override def decode(bt: BType): Result[A] = f(bt)
    }

  implicit def bdecoderMonad: Monad[BDecoder] = new Monad[BDecoder] {
    override def flatMap[A, B](
        fa: BDecoder[A]
    )(f: A => BDecoder[B]): BDecoder[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](
        a: A
    )(f: A => BDecoder[Either[A, B]]): BDecoder[B] = {
      f(a).flatMap {
        case Left(value) => tailRecM(value)(f)
        case Right(v)    => BDecoder.instance(_ => v.asRight)
      }
    }

    override def pure[A](x: A): BDecoder[A] = instance(_ => x.asRight)
  }

  def at[A: BDecoder](key: String): BDecoder[A] =
    instance(
      _.field(key)
        .fold[Result[A]](BencError.CodecError(s"key $key not found").asLeft)(
          BDecoder[A].decode
        )
    )

  def at[A: BDecoder](index: Int): BDecoder[A] = instance {
    case BList(list) =>
      if (index < list.size)
        BDecoder[A].decode(list(index))
      else
        BencError.CodecError(s"Invalid index $index in $list").asLeft
    case v =>
      BencError.CodecError(s"It is not blist. $v").asLeft
  }

  /**
    * BDecoder[BitVector]
    */
  implicit val bitVectorBDecoder: BDecoder[BitVector] = instance(
    _.bstring.toRight(BencError.CodecError("Empty"))
  )

  /**
    * BDecoder[String]
    */
  implicit val utf8StringBDecoder: BDecoder[String] =
    bitVectorBDecoder.emap(
      utf8
        .decode(_)
        .toEither
        .map(_.value)
        .leftMap(err => BencError.CodecError(err.message))
    )

  /**
    * BDecoder[Long]
    */
  implicit val longBDecoder: BDecoder[Long] = instance(
    _.bnum.toRight(BencError.CodecError("Empty"))
  )

  /**
    * BDecoder[Int]
    */
  implicit val intBDecoder: BDecoder[Int] = longBDecoder.map(_.toInt)

  /**
    * BDecoder[List[A]]
    * @tparam A - Decoder for list elements
    */
  implicit def listBDecoder[A: BDecoder]: BDecoder[List[A]] =
    instance(
      _.blist
        .traverse(_.traverse(v => BDecoder[A].decode(v)))
        .flatMap(_.toRight(BencError.CodecError("Empty")))
    )

  trait OptionBDecoder[A] extends BDecoder[A]

  /**
    * BDecoder[Option[A]]
    *
    * @tparam A - Decoder for Option element.
    */
  implicit def optionDEcoder[A: BDecoder]: BDecoder[Option[A]] =
    new OptionBDecoder[Option[A]] {
      override def decode(bt: BType): Result[Option[A]] =
        BDecoder[A].decode(bt).map(Some(_))
    }

  trait HListBDecoder[L <: HList] {
    def decode(ann: Map[String, String]): BDecoder[L]
  }
  object HListBDecoder {
    def instance[L <: HList](
        f: Map[String, String] => BDecoder[L]
    ): HListBDecoder[L] = new HListBDecoder[L] {
      override def decode(ann: Map[String, String]): BDecoder[L] = f(ann)
    }

  }
  implicit val hnilBDncoder: HListBDecoder[HNil] = HListBDecoder.instance { _ =>
    instance(
      _ => HNil.asRight
    )
  }
  implicit def hlistBDecoder[K <: Symbol, H, T <: HList](
      implicit
      fieldName: FieldName,
      witness: Witness.Aux[K],
      henc: Lazy[BDecoder[H]],
      tenc: HListBDecoder[T]
  ): HListBDecoder[FieldType[K, H] :: T] = {
    HListBDecoder.instance { ann =>
      val name =
        ann.getOrElse(witness.value.name, fieldName.name(witness.value))
      instance { bt =>
        val value: Result[H] = (bt.field(name), henc.value) match {
          case (None, _: OptionBDecoder[_]) =>
            None.asInstanceOf[H].asRight[BencError]
          case (None, _) =>
            BencError.CodecError(s"filed $name is missing").asLeft[H]
          case (Some(bt), dec) =>
            dec.decode(bt)
        }
        for {
          head <- value
          tail <- tenc.decode(ann).decode(bt)
        } yield field[K](head) :: tail
      }
    }
  }
  implicit def genericBDecoder[
      A,
      R <: HList,
      D <: HList,
      F <: HList,
      K <: HList
  ](
      implicit
      gen: LabelledGeneric.Aux[A, R],
      underlying: Lazy[HListBDecoder[R]],
      fields: Keys.Aux[R, F],
      fieldsToList: ToTraversable.Aux[F, List, Symbol],
      keys: Annotations.Aux[BencKey, A, K],
      keysToList: ToTraversable.Aux[K, List, Option[BencKey]]
  ): BDecoder[A] = {
    instance { v =>
      val keyAnnotationMap: Map[String, String] =
        fieldsToList(fields())
          .map(_.name)
          .zip(keysToList(keys()))
          .collect {
            case (field, Some(keyAnnotation)) => (field, keyAnnotation.value)
          }
          .toMap
      underlying.value.decode(keyAnnotationMap).decode(v).map(gen.from)
    }
  }

}
