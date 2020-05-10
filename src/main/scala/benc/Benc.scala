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
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import scodec.bits.{ BitVector, ByteVector }
import scodec.codecs._
import scodec.interop.cats._
import scodec.{ Attempt, Codec, DecodeResult, Decoder, Err }

import scala.Function._
import scala.collection.immutable.ListMap
import scala.util.Try

/**
  * Converts from/to binary bencoded value to type `A`
  */
object Benc {

  /**
    * Attempt to convert from binary bencoded value to `A`
    * @param bits - bencoded binary value
    * @return Error if can not to convert or converted value
    */
  def fromBenc[A: BDecoder](bits: BitVector): Result[A] =
    FromBenc.instance.fromBenc(bits).flatMap(_.as[A])

  /**
    * * Attempt to convert from `A`  to binary bencoded value
    * @param a - object to convert
    * @return Error if can not to convert or converted value
    */
  def toBenc[A: BEncoder](a: A): Result[BitVector] =
    BEncoder[A].encode(a).flatMap(ToBenc.instance.toBenc)

}

trait FromBenc {
  def fromBenc(bits: BitVector): Result[BType]
}

object FromBenc {

  lazy val instance: FromBenc = FromBenc()

  def apply(): FromBenc = new FromBenc() {
    override def fromBenc(bits: BitVector): Result[BType] =
      decode(bits).toEither
        .map(_.value)
        .leftMap(err => BencError.CodecError(err.message))

    def decode(bits: BitVector): Attempt[DecodeResult[BType]] = {
      if (bits.isEmpty) {
        Attempt.failure(Err("Nothing to decode"))
      } else {
        byte.decode(bits) match {
          case Attempt.Successful(DecodeResult(b, _)) =>
            b match {
              case 'i'             => step[BNum](bits)
              case 'l'             => step[BList](bits)
              case 'd'             => step[BMap](bits)
              case d if isDigit(d) => step[BString](bits)
              case v               => Attempt.failure(Err(s"Invalid format: $v"))
            }
          case a @ Attempt.Failure(_) => a
        }
      }
    }

    private def step[A <: BType: Decoder](
        b: BitVector
    ): Attempt[DecodeResult[BType]] = {
      val decoder = implicitly[Decoder[A]]
      decoder.decode(b)
    }

    private def isDigit(byte: Byte): Boolean = {
      byte >= 48 && byte <= 57
    }
    private val e: ByteVector = ByteVector("e".getBytes())

    private implicit lazy val bnumDecoder: Decoder[BNum] = Decoder[BNum](
      //value example i333e
      bits => {
        val bytes = bits.bytes.drop(1)
        val idx   = bytes.indexOfSlice(e)
        for {
          num <- limitedSizeBytes(idx, asciiLong).decode(bytes.bits)
        } yield DecodeResult(
          BNum(num.value),
          num.remainder.bytes.drop(e.size).bits
        )
      }
    )

    private implicit lazy val bstringDecoder: Decoder[BString] =
      Decoder[BString] {
        val `:` = ByteVector(":".getBytes)
        bits => {
          val bytes = bits.bytes
          val idx   = bytes.indexOfSlice(`:`)
          for {
            length <- limitedSizeBytes(idx, asciiLong).decode(bytes.bits)
            str = length.remainder.bytes.drop(`:`.size).take(length.value)
          } yield DecodeResult(
            BString(str.bits),
            length.remainder.bytes.drop(`:`.size + str.size).bits
          )
        }
      }

    private def decodeList(
        bits: BitVector,
        list: List[BType]
    ): Attempt[DecodeResult[BList]] = {
      if (bits.startsWith(e.bits)) {
        Attempt.successful(DecodeResult(BList(list), bits.drop(8)))
      } else {
        for {
          dr <- decode(bits)
          d  <- decodeList(dr.remainder, list :+ dr.value)
        } yield d
      }
    }

    private implicit lazy val blistDecoder: Decoder[BList] = Decoder[BList](
      bits => decodeList(bits.drop(8), List.empty)
    )

    private implicit lazy val bmapDecoder: Decoder[BMap] = Decoder[BMap](
      bits => {
        for {
          dr <- decodeList(bits.drop(8), List.empty)
          items <- dr.value.list
            .grouped(2) //first element is a key, second is a value
            .toList
            .traverse {
              case List(BString(bits), v) =>
                ascii.decode(bits).map(_.value).map(key => ListMap(key -> v))
              case List(k, _) =>
                Attempt.failure(Err(s"key should be ascii string: $k"))
            }

          result = items.fold(ListMap.empty[String, BType])(_ ++ _)
        } yield DecodeResult(BMap(result), dr.remainder)

      }
    )

    private val asciiLong: Codec[Long] = ascii.exmap(
      str =>
        Attempt
          .fromTry(Try(str.toLong))
          .mapErr(const(Err(s"Unable decode long: $str"))),
      l => Attempt.successful(l.toString)
    )
  }

}

trait ToBenc {
  def toBenc(bt: BType): Result[BitVector]
}

object ToBenc {
  val instance: ToBenc = ToBenc()

  def apply(): ToBenc = new ToBenc() {
    override def toBenc(bt: BType): Result[BitVector] =
      encodeBType(bt).toEither
        .leftMap(err => BencError.CodecError(err.message))

    val e: BitVector = BitVector("e".getBytes())
    val l: BitVector = BitVector("l".getBytes())
    val d: BitVector = BitVector("d".getBytes())
    val i: BitVector = BitVector("i".getBytes())

    private def encodeBType(v: BType): Attempt[BitVector] = v match {
      case BType.BMap(map) =>
        map
          .foldLeft(Attempt.successful(d)) { (acc, v) =>
            for {
              kk   <- ascii.encode(s"${v._1.length}:${v._1}")
              vv   <- encodeBType(v._2)
              _acc <- acc
            } yield _acc ++ kk ++ vv
          }
          .map(_ ++ e)

      case BType.BNum(num) =>
        for {
          value <- ascii.encode("" + num)
        } yield i ++ value ++ e

      case BType.BList(list) =>
        for {
          bits <- list.traverse(v => encodeBType(v))
          value = bits.fold(BitVector.empty)(_ ++ _)
        } yield l ++ value ++ e

      case BType.BString(bits) =>
        ascii.encode(s"${bits.bytes.size}:").map(_ ++ bits)
    }

  }

}
