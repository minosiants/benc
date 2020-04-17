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
