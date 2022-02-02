package io.navidjalali.gildedrose

sealed trait Rule:
  val qualityChange: Int
  val sellInChange: Int = -1

object Rule:
  case class Decrease(by: Int) extends Rule:
    override val qualityChange: Int = -by

  case class Increase(by: Int) extends Rule:
    override val qualityChange: Int = by

  case object Skip extends Rule:
    override val qualityChange: Int = 0
