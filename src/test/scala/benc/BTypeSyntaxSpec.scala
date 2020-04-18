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
    "toBenc" in Prop.forAll(nameGen) { name =>
      val result = BEncoder[Name].encode(name).flatMap(_.toBenc)

      val expected = Benc.toBenc(name)
      result.isRight ==== true
      result ==== expected
    }
  }

}

object BTypeSyntaxSpec {
  final case class Name(name: String)
  val nameGen: Gen[Name] = Gen.alphaStr.map(Name)
}
