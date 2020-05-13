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

import benc.BType.{ BMap, BString }
import cats.syntax.either._
import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scodec.bits.BitVector

class CodecSpec extends Specification with ScalaCheck {
  import CodecSpec._

  "Product" should {
    "be encoded" in Prop.forAll(bookGen) { book =>
      val result =
        BEncoder[Book].encode(book).flatMap(bt => BDecoder[Book].decode(bt))

      result ==== book.asRight
    }

    "be encoded by codec" in Prop.forAll(bookGen) { book =>
      val codec = BCodec[Book]
      val result =
        codec.encode(book).flatMap(bt => codec.decode(bt))
      result ==== book.asRight
    }

    "be encoded by custom codec" in Prop.forAll(bookGen) { book =>
      val be            = BEncoder[Book]
      implicit val idbd = BDecoder.instance[Id](_ => Id("hello").asRight)

      val bd: BDecoder[Book] = for {
        id      <- BDecoder.at[Id]("id")
        author  <- BDecoder.at[Author]("author")
        content <- BDecoder.at[BitVector]("content")
        pages   <- BDecoder.at[Long]("pages")
      } yield Book(id, author, content, pages)

      val codec: BCodec[Book] = BCodec.instance(be, bd)

      val result =
        codec.encode(book).flatMap(bt => codec.decode(bt))
      result ==== book.copy(id = Id("hello")).asRight
    }

    "keyname annotation" in {
      val newBrand = "brand2"
      case class Pen(@BencKey(newBrand) brand: String)
      val codec: BCodec[Pen] = BCodec[Pen]
      val result = for {
        v <- codec.encode(Pen("bic"))
        _ <- v.bmap.flatMap(_.get(newBrand)).toRight(BencError.NotFound)
        _ <- codec.decode(v)
      } yield ()
      result.isRight
    }

    "bencIgnore annotation" in {
      case class Pen(brand: String, @BencIgnore hello: String)
      val codec: BCodec[Pen] = BCodec[Pen]
      val result             = codec.encode(Pen("bic", "hello"))
      result ==== BType
        .singleBMap("brand", BType.stringBString("bic"))
        .asRight[BencError]
    }
    "at option with some value" in {
      val bmap   = BType.singleBMap("brand", BType.stringBString("bic"))
      val result = BDecoder.at[Option[String]]("brand").decode(bmap)

      result ==== Some("bic").asRight[BencError]
    }

    "at option with none value" in {
      val bmap   = BType.emptyBMap
      val result = BDecoder.at[Option[String]]("brand").decode(bmap)

      result ==== None.asRight[BencError]
    }

    "custom fieldName in encoder" in Prop.forAll(idGen) { id =>
      implicit object upperCaseFiledName extends FieldName {
        override def name[K <: Symbol](k: K): String = k.name.toUpperCase
      }
      val result = BEncoder[Id].encode(id)

      result ==== BMap(Map("ID" -> BString(BitVector(id.id.getBytes())))).asRight
    }

    "custom fieldName in decoder" in Prop.forAll(idGen) { id =>
      implicit object upperCaseFiledName extends FieldName {
        override def name[K <: Symbol](k: K): String = k.name.toUpperCase
      }
      val result =
        BMap(Map("ID" -> BString(BitVector(id.id.getBytes())))).as[Id]
      result ==== id.asRight
    }
  }
}

object CodecSpec {
  final case class Id(id: String)
  final case class Author(name: String, age: Option[Int])
  final case class Book(id: Id, author: Author, content: BitVector, pages: Long)

  val bitVectorGen: Gen[BitVector] =
    Gen.alphaStr.map(v => BitVector(v.getBytes()))

  val idGen: Gen[Id] = Gen.alphaStr.map(Id)

  val authorGen: Gen[Author] = for {
    name <- Gen.alphaStr
    age  <- Gen.option(Gen.posNum[Int])
  } yield Author(name, age)

  val bookGen: Gen[Book] = for {
    id      <- idGen
    author  <- authorGen
    content <- bitVectorGen
    pages   <- Gen.posNum[Long]
  } yield Book(id, author, content, pages)

}
