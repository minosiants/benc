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

import benc.BDecoder.{ HListBDecoder, OptionBDecoder }
import benc.BEncoder.{ HListBEncoder, OptionBEncoder }
import cats.syntax.either._
import scodec.bits.BitVector
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Keys

/**
  * Interface over BEncoder and BDecoder
  */
trait BCodec[A] extends BEncoder[A] with BDecoder[A] {

  final def exmap[B](f: A => Result[B], g: B => Result[A]): BCodec[B] =
    BCodec.instance[B](
      (b: B) => econtramap(g).encode(b),
      (bt: BType) => emap(f).decode(bt)
    )

  final def xmap[B](f: A => B, g: B => A): BCodec[B] = BCodec.instance[B](
    (b: B) => encode(g(b)),
    (bt: BType) => decode(bt).map(f)
  )

}

object BCodec {

  def apply[A](implicit F: BCodec[A]): BCodec[A] = F

  def instance[A](enc: A => Result[BType], dec: BType => Result[A]): BCodec[A] =
    new BCodec[A] {
      override def encode(a: A): Result[BType]  = enc(a)
      override def decode(bt: BType): Result[A] = dec(bt)
    }

  def instance[A](enc: BEncoder[A], dec: BDecoder[A]): BCodec[A] =
    new BCodec[A] {
      override def encode(a: A): Result[BType]  = enc.encode(a)
      override def decode(bt: BType): Result[A] = dec.decode(bt)
    }

  implicit val bitVectorBCodec: BCodec[BitVector] = instance(
    BEncoder.bitVectorBEncoder,
    BDecoder.bitVectorBDecoder
  )

  implicit val stringBCodec: BCodec[String] = instance(
    BEncoder.stringBEncoder,
    BDecoder.utf8StringBDecoder
  )

  implicit val longBCodec: BCodec[Long] = instance(
    BEncoder.longBEncoder,
    BDecoder.longBDecoder
  )

  implicit val intBCodec: BCodec[Int] = instance(
    BEncoder.intBEncoder,
    BDecoder.intBDecoder
  )

  implicit def listBCodec[A: BEncoder: BDecoder]: BCodec[List[A]] = instance(
    BEncoder.listBEncoder[A],
    BDecoder.listBDecoder[A]
  )

  trait OptionBCodec[A]
      extends OptionBEncoder[A]
      with OptionBDecoder[A]
      with BCodec[A]

  implicit def optionBCodec[A: BEncoder: BDecoder]: BCodec[Option[A]] =
    new OptionBCodec[Option[A]] {
      override def encode(a: Option[A]): Result[BType] =
        BEncoder.optionBEncoder[A] match {
          case e: OptionBEncoder[_] => e.encode(a)
          case _                    => BencError.CodecError("Should not be here ").asLeft
        }
      override def decode(bt: BType): Result[Option[A]] =
        BDecoder.optionDEcoder[A].decode(bt)
    }

  trait HListBCodec[L <: HList] extends HListBEncoder[L] with HListBDecoder[L]

  object HListBCodec {
    def instance[L <: HList](
        enc: HListBEncoder[L],
        dec: HListBDecoder[L]
    ): HListBCodec[L] = new HListBCodec[L] {
      override def decode(ann: Map[String, String]): BDecoder[L] =
        dec.decode(ann)

      override def encode(
          keys: Map[String, String],
          ignors: Map[String, Boolean]
      ): BEncoder[L] =
        enc.encode(keys, ignors)
    }

    implicit val hnilCodec: HListBCodec[HNil] = HListBCodec.instance(
      BEncoder.hnilEncoder,
      BDecoder.hnilBDncoder
    )

    implicit def hlistBCodec[K <: Symbol, H, T <: HList](
        implicit
        fieldName: FieldName,
        witness: Witness.Aux[K],
        henc: Lazy[BCodec[H]],
        tenc: HListBCodec[T]
    ): HListBCodec[FieldType[K, H] :: T] =
      HListBCodec.instance(
        BEncoder.hlistEncoder[K, H, T],
        BDecoder.hlistBDecoder[K, H, T]
      )
  }

  implicit def genericBCodec[
      A,
      R <: HList,
      D <: HList,
      F <: HList,
      K <: HList,
      T <: HList
  ](
      implicit
      gen: LabelledGeneric.Aux[A, R],
      underlyingE: Lazy[HListBDecoder[R]],
      underlyingD: Lazy[HListBEncoder[R]],
      fields: Keys.Aux[R, F],
      fieldsToList: ToTraversable.Aux[F, List, Symbol],
      keys: Annotations.Aux[BencKey, A, K],
      keysToList: ToTraversable.Aux[K, List, Option[BencKey]],
      ignors: Annotations.Aux[BencIgnore, A, T],
      ignorsToList: ToTraversable.Aux[T, List, Option[BencIgnore]]
  ): BCodec[A] = instance(
    BEncoder.genericEncoder[A, R, D, F, K, T],
    BDecoder.genericBDecoder[A, R, D, F, K]
  )

}
