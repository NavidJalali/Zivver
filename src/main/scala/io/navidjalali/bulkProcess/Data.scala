package io.navidjalali.bulkProcess

import zio.Chunk

import scala.util.Try

case class Data(productId: Int, place: String):
  def toChunk: Chunk[Byte] = Chunk.fromArray(toString.getBytes())

  override def toString: String = s"""$productId, "$place""""

object Data:
  private val chars = ('A' to 'Z').toArray

  def random: Data = Data(scala.util.Random.nextInt(64), s"$randomChar$randomChar")

  private def randomChar = chars(scala.util.Random.nextInt(26))

  def fromString(str: String): Option[Data] =
    str.split(',') match {
      case Array(id, cc) =>
        for {
          pid <- id.toIntOption
          code <- Try(cc.dropRight(1).drop(2).ensuring(_.length == 2))
            .toOption
        } yield Data(pid, code)
      case _ => None
    }
