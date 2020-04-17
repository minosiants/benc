package benc

import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import cats.syntax.either._

class BTypeSyntaxSpec extends Specification with ScalaCheck {
  import BTypeSyntaxSpec._

  "BTypeSyntax" should {

    "as" in Prop.forAll(nameGen) { name =>
      val result = BEncoder[Name].encode(name).flatMap(_.as[Name])
      result ==== name.asRight
    }
    "toBin" in Prop.forAll(nameGen) { name =>
      val result = BEncoder[Name].encode(name).flatMap(_.toBin)
      val expected = for {
        v   <- BEncoder[Name].encode(name)
        bin <- BType.toBin(v)
      } yield bin

      result.isRight ==== true
      result ==== expected
    }
  }

}

object BTypeSyntaxSpec {
  final case class Name(name: String)
  val nameGen: Gen[Name] = Gen.alphaStr.map(Name)
}
