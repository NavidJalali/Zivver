package io.navidjalali.gildedrose

import io.navidjalali.gildedrose.GildedRose
import io.navidjalali.gildedrose.Items.*
import io.navidjalali.gildedrose.goblin.Item
import zio.Random
import zio.test.*
import zio.test.Assertion.*

object GildedRoseSpecs extends DefaultRunnableSpec:

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Gilded Rose")(
      test("Once the sell by date has passed, Quality degrades twice as fast") {
        check(Gen.int(4, 50)) {
          quality =>
            val gildedRose = GildedRose(Array(new Item("foo", 1, quality)))
            gildedRose.updateQuality()
            val first = gildedRose.items(0).quality
            gildedRose.updateQuality()
            val second = gildedRose.items(0).quality
            assertTrue((first - second) / (quality - first) == 2)
        }
      },
      test("The Quality of an item is never negative") {
        check(Gen.int(10, 100), Gen.int(1, 100).flatMap(Gen.chunkOfN(_)(genItem()))) {
          case (epochs, items) =>
            val gildedRose = GildedRose(items.toArray)
            (0 to epochs).foreach(_ => gildedRose.updateQuality())
            assertTrue(gildedRose.items.forall(_.quality >= 0))
        }
      },
      test("'Aged Brie' actually increases in Quality the older it gets") {
        val agedBrie = new Item("Aged Brie", 5, 10)
        val gildedRose = GildedRose(Array(agedBrie))
        val quality = (0 to 20).map {
          _ =>
            gildedRose.updateQuality()
            gildedRose.items(0).quality
        }
        assertTrue(quality.zip(quality.tail).forall {
          _ < _
        })
      },
      test("Sulfuras, Hand of Ragnaros has constant quality") {
        val sulfuras = new Item("Sulfuras, Hand of Ragnaros", 10, 80)
        val gildedRose = GildedRose(Array(sulfuras))
        val quality = (0 to 100).map {
          _ =>
            gildedRose.updateQuality()
            gildedRose.items(0).quality
        }
        assertTrue(quality.forall(_ == 80))
      },
      suite("Backstage Passes")(
        test("Never decrease in quality until after the concert") {
          val pass = new Item("Backstage passes to a TAFKAL80ETC concert", 20, 10)
          val gildedRose = GildedRose(Array(pass))
          val quality = (0 to 20).map {
            _ =>
              gildedRose.updateQuality()
              gildedRose.items(0).quality
          }

          val neverDecrease = quality.zip(quality.tail).dropRight(1).forall {
            _ < _
          }

          assertTrue(neverDecrease) && assertTrue(quality.last == 0)
        },
        test("Quality increases by 2 when there are between 5 to 10 days left") {
          val pass = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 1)
          val gildedRose = GildedRose(Array(pass))
          val quality = (0 to 5).map {
            _ =>
              gildedRose.updateQuality()
              gildedRose.items(0).quality
          }

          assertTrue(quality == (1 to 6).map(_ * 2))
        },
        test("Quality increases by 2 when there are between 5 to 0 days left") {
          val pass = new Item("Backstage passes to a TAFKAL80ETC concert", 6, 1)
          val gildedRose = GildedRose(Array(pass))
          val quality = (0 to 5).map {
            _ =>
              gildedRose.updateQuality()
              gildedRose.items(0).quality
          }

          assertTrue(quality == (1 to 6).map(_ * 3))
        },
      ),
      suite("Conjured")(
        test("Conjured items decrease in quality twice as fast") {
          val manaCake = new Item("Mana Cake", 10, 10)
          val conjuredManaCake = new Item("Conjured Mana Cake", 10, 10)
          val gildedRose = GildedRose(Array(manaCake, conjuredManaCake))
          gildedRose.updateQuality()
          assertTrue((gildedRose.items(1).quality - 10) / (gildedRose.items(0).quality - 10) == 2)
        },
        test("Conjured version of item that does not decrease in quality also does not decrease in quality") {
          val sulfuras = new Item(Items.SULFURAS, 10, 80)
          val conjuredSulfuras = new Item(s"Conjured ${Items.SULFURAS}", 10, 80)
          val gildedRose = GildedRose(Array(sulfuras, conjuredSulfuras))
          check(Gen.int(5, 100)) { i =>
            (0 to i).foreach(_ => gildedRose.updateQuality())
            assertTrue(gildedRose.items.forall(_.quality == 80))
          }
        },
        test("Conjured version of items that increase in quality also increase in quality") {
          val brie = new Item("Aged Brie", 10, 10)
          val conjuredBrie = new Item("Conjured Aged Brie", 10, 10)
          val gildedRose = GildedRose(Array(brie, brie))
          gildedRose.updateQuality()
          assertTrue(gildedRose.items(0).quality == gildedRose.items(1).quality)
        }
      )
    )

  def genItem(sellInMin: Int = 1,
              sellInMax: Int = 20,
              qualityMin: Int = 0,
              qualityMax: Int = 50) =
    for {
      name <- Gen.string(Gen.char)
      sellIn <- Gen.int(sellInMin, sellInMax)
      quality <- Gen.int(qualityMin, qualityMax)
    } yield new Item(name, sellIn, quality)
