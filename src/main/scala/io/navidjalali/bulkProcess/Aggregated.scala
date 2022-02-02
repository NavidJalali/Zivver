package io.navidjalali.bulkProcess

import scala.util.Try

case class Aggregated(productId: Int, places: Set[String]):
  override def toString: String = s"""$productId -> ${places.map(cc => s""""$cc"""").mkString("[", ", ", "]")}"""

object Aggregated:
  def fromString(str: String): Option[Aggregated] =
    str.split(" -> ") match {
      case Array(pid, codes) =>
        for {
          id <- pid.toIntOption
          cc <- Try(
            codes.drop(1).dropRight(1).split(", ").map(_.drop(1).dropRight(1)).toSet
          ).toOption
        } yield Aggregated(id, cc)
      case _ => None
    }
