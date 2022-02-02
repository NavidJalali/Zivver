package io.navidjalali.gildedrose

import io.navidjalali.gildedrose.goblin.Item

final case class Update(run: Item => Option[Rule]) {
  self =>
  def when(predicate: Item => Boolean): Update = Update.when(predicate)(this)

  def ++(that: Update): Update = or(that)

  def or(that: Update): Update = Update(
    item => self.run(item) match {
      case Some(result) => Some(result)
      case None => that.run(item)
    }
  )

  def flatMap(f: Rule => Update): Update = Update(
    item => self.run(item) match {
      case Some(result) => f(result).run(item)
      case None => None
    }
  )

  def map(f: Rule => Rule): Update = Update(
    item => self.run(item).map(f)
  )
}

object Update:
  def fail: Update = Update(_ => None)

  def succeed(rule: Rule): Update = Update(item => Some(rule))

  def when(predicate: Item => Boolean)(update: Update): Update =
    Update(item => if (predicate(item)) update.run(item) else None)
