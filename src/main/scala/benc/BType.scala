package benc

import scodec.bits.BitVector

import scala.Function._
import scala.collection.immutable.ListMap

abstract sealed class BType extends Product with Serializable {

  def fold[A](
      bstring: BitVector => A,
      bnum: Long => A,
      blist: List[BType] => A,
      bmap: Map[String, BType] => A
  ): A = this match {
    case BType.BString(bits) => bstring(bits)
    case BType.BNum(value)   => bnum(value)
    case BType.BList(list)   => blist(list)
    case BType.BMap(map)     => bmap(map)
  }
  def bstring: Option[BitVector] =
    fold(Some(_), const(None), const(None), const(None))

  def bnum: Option[Long] = fold(const(None), Some(_), const(None), const(None))

  def blist: Option[List[BType]] =
    fold(const(None), const(None), Some(_), const(None))

  def bmap: Option[Map[String, BType]] =
    fold(const(None), const(None), const(None), Some(_))

}

object BType {
  final case class BString(bits: BitVector)    extends BType
  final case class BNum(value: Long)           extends BType
  final case class BList(list: List[BType])    extends BType
  final case class BMap(m: Map[String, BType]) extends BType

  object BMap {
    val empty = BMap(ListMap.empty[String, BType])
  }

  def fromBin(bits: BitVector): Result[BType] = FromBin.instance.fromBin(bits)
  def toBin(bt: BType): Result[BitVector] =
    ToBin.instance.toBin(bt)
}
