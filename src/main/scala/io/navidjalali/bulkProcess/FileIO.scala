package io.navidjalali.bulkProcess

import io.navidjalali.bulkProcess.FileIO.Error
import zio.stream.{ZPipeline, ZSink, ZStream}
import zio.{Chunk, IO, ZIO, ZLayer}

import java.io.File

trait FileIO:
  def write[A](path: String)(data: ZStream[Any, Error, A]): IO[Error, String]

  def read[A](path: String)(decode: String => Option[A]): ZStream[Any, Error.ReadError, A]

  def delete(path: String): IO[Error.DeleteError, Unit]

object FileIO:

  val live = ZLayer.succeed(
    new FileIO {
      override def write[A](path: String)(data: ZStream[Any, Error, A]): IO[Error, String] =
        data
          .flatMap(a => ZStream.fromChunk(Chunk.fromArray((a.toString + '\n').getBytes())))
          .run(ZSink.fromFile(new File(path)).mapError(Error.WriteError(_)))
          .as(path)

      override def read[A](path: String)(decode: String => Option[A]): ZStream[Any, Error.ReadError, A] =
        ZStream.fromFile(new File(path))
          .via(ZPipeline.utf8Decode)
          .via(ZPipeline.splitLines)
          .map(decode)
          .collect { case Some(data) => data }
          .mapError(Error.ReadError(_))

      override def delete(path: String): IO[Error.DeleteError, Unit] =
        ZIO.attempt(new File(path).delete())
          .mapError(Error.DeleteError(_))
          .flatMap(success =>
            ZIO.fail(Error.DeleteError(new RuntimeException(s"Unsuccessful delete: $path")))
              .unless(success)
          )
          .unit
    }
  )

  sealed trait Error

  object Error {
    case class WriteError(cause: Throwable) extends Error

    case class ReadError(cause: Throwable) extends Error

    case class DeleteError(cause: Throwable) extends Error
  }
