package io.navidjalali.gildedrose.goblin

class Item(val name: String, var sellIn: Int, var quality: Int)

object Item:
  def unapply(item: Item): Some[(String, Int, Int)] = Some((item.name, item.sellIn, item.quality))
