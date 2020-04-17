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

import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scodec.bits.BitVector
import BType._

class BinSpec extends Specification with ScalaCheck {
  import BinSpec._

  "BTypeCodec" should {
    "encode bnum" in prop[BNum]

    "encode bstring" in prop[BString]

    "encode blist" in prop[BList]

    "encode bmap" in prop[BMap]

  }

  def prop[A <: BType: Gen]: Prop = {
    val gen = implicitly[Gen[A]]
    forAll(gen) { bt =>
      val encoded = BType.toBin(bt)
      val decoded = encoded.flatMap(BType.fromBin)
      decoded ==== Right(bt)
    }
  }

}

object BinSpec {

  implicit val bstringGen: Gen[BString] = for {
    str <- Gen.asciiPrintableStr
  } yield BString(BitVector(str.getBytes))

  implicit val bnumGen: Gen[BNum] =
    Gen.oneOf(Gen.posNum[Long], Gen.negNum[Long]).map(BNum)

  implicit val blistGen: Gen[BList] = Gen
    .nonEmptyListOf(
      Gen.oneOf(
        bstringGen,
        bnumGen
      )
    )
    .map(BList)

  val bmapPairGen: Gen[(String, BType)] = for {
    key          <- Gen.asciiPrintableStr
    value: BType <- Gen.oneOf(bstringGen, bnumGen, blistGen)
  } yield (key, value)

  implicit val bmapGen: Gen[BMap] = Gen.mapOf(bmapPairGen).map(BMap(_))

}
