package benc

import cats.Show

import scala.util.control.NoStackTrace

sealed abstract class Error extends NoStackTrace with Product with Serializable

object Error {
  final case object NotFound               extends Error
  final case class CodecError(msg: String) extends Error

  implicit val showError: Show[Error] = Show.show {
    case NotFound        => "Not found"
    case CodecError(msg) => s"Codec error: $msg"
  }
}
