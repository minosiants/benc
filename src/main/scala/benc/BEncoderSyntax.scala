package benc

trait BEncoderSyntax {

  implicit def bencoderSyntax[A](a: A): BEncoderOps[A] = new BEncoderOps(a)
}

final class BEncoderOps[A](private val a: A) extends AnyVal {
  def asBType(implicit encoder: BEncoder[A]): Result[BType] = encoder.encode(a)
}
