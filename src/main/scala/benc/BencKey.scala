package benc

import scala.annotation.Annotation

/**
  * Annotation used to specify encoding key name in case class
  */
case class BencKey(value: String) extends Annotation
