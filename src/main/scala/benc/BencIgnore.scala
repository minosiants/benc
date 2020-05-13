package benc

import scala.annotation.Annotation

/**
  * Annotation used to ignore property during encoding
  */
case class BencIgnore() extends Annotation
