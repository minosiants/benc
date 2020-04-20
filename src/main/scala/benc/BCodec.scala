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

import benc.BDecoder.{ BMapDecoder, OptionBDecoder }
import benc.BEncoder.{ BMapEncoder, OptionBEncoder }
import benc.BType.BMap
import scodec.bits.BitVector
import shapeless._
import shapeless.labelled.FieldType
import cats.syntax.either._

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

  trait BMapCodec[A] extends BMapEncoder[A] with BMapDecoder[A] {}

  def bmapCodecinstance[A](
      enc: BMapEncoder[A],
      dec: BMapDecoder[A]
  ): BMapCodec[A] =
    new BMapCodec[A] {
      override def encode(a: A): Result[BMap] = enc.encode(a)

      override def decodeBMap(bm: BMap): Result[A] = dec.decodeBMap(bm)
    }

  implicit val hnilCodec: BMapCodec[HNil] = bmapCodecinstance(
    BEncoder.hnilEncoder,
    BDecoder.hnilBDncoder
  )

  implicit def hlistBCodec[K <: Symbol, H, T <: HList](
      implicit
      fieldName: FieldName,
      witness: Witness.Aux[K],
      henc: Lazy[BCodec[H]],
      tenc: BMapCodec[T]
  ): BMapCodec[FieldType[K, H] :: T] =
    bmapCodecinstance(
      BEncoder.hlistEncoder[K, H, T],
      BDecoder.hlistBDecoder[K, H, T]
    )

  implicit def genericBCodec[A, H](
      implicit
      gen: LabelledGeneric.Aux[A, H],
      hencoder: Lazy[BMapCodec[H]]
  ): BCodec[A] = instance(
    BEncoder.genericEncoder[A, H],
    BDecoder.genericBDecoder[A, H]
  )
}
