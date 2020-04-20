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
  //def apply[A](implicit E: BEncoder[A], D: BDecoder[A]): BCodec[A] = instance(E,D)
  def apply[A](implicit E: BCodec[A]): BCodec[A] = E

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
    BDecoder.bitVectorDecoder
  )

  implicit val stringBCodec: BCodec[String] = instance(
    BEncoder.stringBEncoder,
    BDecoder.utf8StringDecoder
  )

  implicit val longBCodec: BCodec[Long] = instance(
    BEncoder.longBEncoder,
    BDecoder.longDecoder
  )

  implicit val intBCodec: BCodec[Int] = instance(
    BEncoder.intBEncoder,
    BDecoder.intDecoder
  )

  implicit def listBCodec[A: BEncoder: BDecoder]: BCodec[List[A]] = instance(
    BEncoder.listBEncoder[A],
    BDecoder.listDecoder[A]
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
    BDecoder.hnilDncoder
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
      BDecoder.hlistDecoder[K, H, T]
    )

  implicit def genericBCodec[A, H](
      implicit
      gen: LabelledGeneric.Aux[A, H],
      hencoder: Lazy[BMapCodec[H]]
  ): BCodec[A] = instance(
    BEncoder.genericEncoder[A, H],
    BDecoder.genericDecoder[A, H]
  )
}
