package io.navidjalali.gildedrose

import io.navidjalali.gildedrose.goblin.Item

object Items {
  val AGED_BRIE = "Aged Brie"
  val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  val SULFURAS = "Sulfuras, Hand of Ragnaros"

  val default: Update =
    Update.succeed(Rule.Decrease(2))
      .when(_.isExpired)
      .or(Update.succeed(Rule.Decrease(1)))

  val agedBrie: Update =
    Update.succeed(Rule.Increase(1))
      .when(_.name == Items.AGED_BRIE)

  val backstagePass: Update = Update(
    item => Some(
      if (item.sellIn > 10) Rule.Increase(1)
      else if (item.sellIn > 5) Rule.Increase(2)
      else if (item.sellIn > 0) Rule.Increase(3)
      else Rule.Decrease(item.quality)
    )
  )
    .when(_.name == Items.BACKSTAGE_PASS)

  val sulfuras: Update = Update.succeed(Rule.Skip).when(_.name == Items.SULFURAS)

  val nonConjured: Update = agedBrie ++ backstagePass ++ sulfuras ++ default

  val conjured: Update = Update(
    item =>
      if (item.name.startsWith("Conjured ")) {
        nonConjured.run(new Item(item.name.drop(9), item.sellIn, item.quality)).map {
          case Rule.Decrease(by) => Rule.Decrease(by * 2)
          case other => other
        }
      } else None
  )

  val updateItem: Update = conjured ++ nonConjured

  private def clampQuality(item: Item) = item match {
    case Item("Sulfuras, Hand of Ragnaros", _, _) | Item("Conjured Sulfuras, Hand of Ragnaros", _, _) =>
      clamp(0, 80)
    case _ =>
      clamp(0, 50)
  }

  private def clamp(min: Int, max: Int)(value: Int) = Math.max(min, Math.min(max, value))

  private def clampSellIn = clamp(0, Int.MaxValue)

  extension (item: Item)
    def asString: String = s"Item(${item.name}, ${item.sellIn}, ${item.quality})"
    def isExpired: Boolean = item.sellIn <= 0
    def rule: Option[Rule] = updateItem.run(item)
    def update(): Unit =
      rule.foreach(rule => {
        item.quality = clampQuality(item)(item.quality + rule.qualityChange)
        item.sellIn = clampSellIn(item.sellIn + rule.sellInChange)
      })
}
