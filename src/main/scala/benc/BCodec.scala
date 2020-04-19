package benc

import scodec.bits.BitVector

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

  val bitVectorBCodec: BCodec[BitVector] = instance(
    BEncoder.bitVectorBEncoder,
    BDecoder.bitVectorDecoder
  )

  val stringBCodec: BCodec[String] = instance(
    BEncoder.stringBEncoder,
    BDecoder.utf8StringDecoder
  )

  val longBCodec: BCodec[Long] = instance(
    BEncoder.longBEncoder,
    BDecoder.longDecoder
  )

  val intBCodec: BCodec[Int] = instance(
    BEncoder.intBEncoder,
    BDecoder.intDecoder
  )

  def listBCodec[A: BEncoder: BDecoder]: BCodec[List[A]] = instance(
    BEncoder.listBEncoder[A],
    BDecoder.listDecoder[A]
  )

  def optionBCodec[A: BEncoder: BDecoder]: BCodec[Option[A]] = instance(
    BEncoder.optionBencoder[A],
    BDecoder.optionDecoder[A]
  )

}
