package io.navidjalali.gildedrose

import io.navidjalali.gildedrose.Items.*
import io.navidjalali.gildedrose.goblin.Item

final case class GildedRose(items: Array[Item]):
  def updateQuality(): Unit = items.foreach(_.update())

