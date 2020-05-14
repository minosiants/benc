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

  /**
    * Snake Case 'snake_case'
    * https://en.wikipedia.org/wiki/Snake_case
    */
  object snakeCaseFieldName extends FieldName {
    override def name[K <: Symbol](k: K): String = {
      k.name.toList.foldLeft("") { (acc, ch) =>
        if (ch.isUpper)
          s"${acc}_${ch.toLower}"
        else
          s"$acc$ch"
      }
    }
  }

}
