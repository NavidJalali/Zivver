package io.navidjalali.bulkProcess

import zio.{Console, ZEnv, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault:
  val app: ZIO[BulkProccess, FileIO.Error, Unit] =
    for {
      processor <- ZIO.service[BulkProccess]
      _ <- ZIO.debug("Writing random data...")
      _ <- processor.writeRandom("data.csv", 1000000)
      _ <- ZIO.debug("Wrote random data to data.csv")
      _ <- processor.process(inputPath = "data.csv", chunkSize = 256)
    } yield ()

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    app
      .catchAllCause(cause => Console.printLine(s"Error encountered: $cause"))
      .provide(BulkProccess.live, FileIO.live, Console.live)
