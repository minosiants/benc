package benc

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scodec.bits.BitVector

import org.scalacheck._
import cats.syntax.either._
class CodecSpec extends Specification with ScalaCheck {
  import CodecSpec._

  "Product" should {
    "be encoded" in Prop.forAll(bookGen) { book =>
      val result =
        BEncoder[Book].encode(book).flatMap(bt => BDecoder[Book].decode(bt))

      result ==== book.asRight
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
