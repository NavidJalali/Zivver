package io.navidjalali.bulkProcess

import zio.stream.ZStream
import zio.{Accessible, Chunk, Function1ToLayerOps, IO, ZIO}

import java.util.UUID

trait BulkProccess:
  def writeRandom(path: String, amount: Int): IO[FileIO.Error, String]

  def writeSubResults(inputPath: String): IO[FileIO.Error, Chunk[String]]

  def merge(x: ZStream[Any, FileIO.Error, Aggregated],
            y: ZStream[Any, FileIO.Error, Aggregated]): ZStream[Any, FileIO.Error, Aggregated]

  def reduce(files: List[String]): IO[FileIO.Error, String]

  def process(inputPath: String): IO[FileIO.Error, String]

  protected def processChunk(chunk: Chunk[Data]): Chunk[Aggregated]


object BulkProccess extends Accessible[BulkProccess]:

  val live = (BulkProcessLive.apply _).toLayer

  case class BulkProcessLive(fileIO: FileIO) extends BulkProccess:
    override def writeSubResults(inputPath: String): IO[FileIO.Error, Chunk[String]] =
      fileIO.read(inputPath)(Data.fromString)
        .grouped(256)
        .map(processChunk)
        .zipWithIndex
        .map { case (chunk, index) =>
          fileIO.write(index.toString)(ZStream.fromChunk(chunk)).as(index.toString)
        }
        .flatMap(ZStream.fromZIO(_))
        .runCollect

    override def writeRandom(path: String, amount: Int): IO[FileIO.Error, String] =
      fileIO.write(path)(ZStream.fromIterable(0 to amount).as(Data.random))

    def merge(x: ZStream[Any, FileIO.Error, Aggregated],
              y: ZStream[Any, FileIO.Error, Aggregated]): ZStream[Any, FileIO.Error, Aggregated] =
      x.map { case Aggregated(productId, availableIn) => (productId, availableIn) }
        .zipAllSortedByKeyWith(y.map { case Aggregated(productId, availableIn) => (productId, availableIn) })(identity, identity)(_ union _)
        .map((Aggregated.apply _).tupled)

    def reduce(files: List[String]): IO[FileIO.Error, String] =
      if (files.isEmpty) ZIO.die(new RuntimeException("Empty dataset encoutered."))
      else if (files.length == 1) ZIO.succeed(files.head)
      else
        ZIO.foreach(files.grouped(2).toList) {
          case List(a) => ZIO.succeed(a)
          case List(a, b) =>
            fileIO.write(UUID.randomUUID.toString)(
              merge(fileIO.read(a)(Aggregated.fromString), fileIO.read(b)(Aggregated.fromString))
            ).zipLeft(fileIO.delete(a) <&> fileIO.delete(b))
          case other => ZIO.die(new RuntimeException(s"Unexpected grouping: $other"))
        }
          .flatMap(reduce)

    override def process(inputPath: String): IO[FileIO.Error, String] =
      writeSubResults(inputPath)
        .debug("Sub-Results: ")
        .map(_.toList)
        .flatMap(reduce)
        .debug("Final Result: ")

    override protected def processChunk(chunk: Chunk[Data]): Chunk[Aggregated] =
      Chunk.fromIterable(
        chunk
          .groupBy(_.productId)
          .map { case (key, value) => Aggregated(key, value.map(_.place).toSet) }
      )
        .sortBy(_.productId)
