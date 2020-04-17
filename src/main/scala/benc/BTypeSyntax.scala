package benc

import scodec.bits.BitVector

trait BTypeSyntax {
  implicit def btypeSyntax(bt: BType): BTypeOps = new BTypeOps(bt)
}

final class BTypeOps(val bt: BType) extends AnyVal {
  def toBin: Result[BitVector]   = BType.toBin(bt)
  def as[A: BDecoder]: Result[A] = BDecoder[A].decode(bt)
}
