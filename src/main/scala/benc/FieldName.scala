package benc

/**
  * Strategy for field name during encoding/decoding
  */
trait FieldName {
  def name[K <: Symbol](k: K): String
}

object FieldName {

  implicit object defaultFieldName extends FieldName {
    override def name[K <: Symbol](k: K): String = k.name
  }

}
