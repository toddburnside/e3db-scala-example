package example.e3dbs

import java.util.{Map => JMap}

import scala.collection.JavaConverters._
import cats.implicits._
import cats.effect._
import com.tozny.e3db._

import MetaSplitter._

// This contains the wrapped calls to the Java API. Only a small subset of the API
// is currently wrapped, of course.
object E3dbs {

  def register(token: String, clientName: String, host: String): IO[Config] =
    // async() converts the callback based API to IO.
    Effect[IO].async((cb: Callback[Config]) => registrar(token, clientName, host, cb))

  // Writes the data to e3db without any metadata - in other words, all encrypted.
  def writeNoMeta[A](client: Client, recordType: String, data: A)(
      implicit encoder: MapEncoder[A]): IO[Record] = {
    val map = encoder.encode(data).asJava
    write(client, recordType, map, null)
  }

  // Write the data to e3db, but any data members of Data that are also in Meta will
  // be metadata, and not encrypted.
  def write[Data, Meta](client: Client, recordType: String, data: Data)(
      implicit splitter: MetaSplitter[Data, Meta]
  ): IO[Record] = {
    val (dataMap, metaMap) = data.splitMeta[Meta]
    write(client, recordType, dataMap.asJava, metaMap.asJava)
  }

  // The only lightly wrapped API that takes Java Maps.
  def write(client: Client,
            recordType: String,
            data: JMap[String, String],
            meta: JMap[String, String]): IO[Record] =
    Effect[IO].async((cb: Callback[Record]) => writer(client, recordType, data, meta, cb))

  // Query e3db. This returns the tozny QueryResponse in an IO. QueryResponse is ugly,
  // containing the Java Maps, but I provide an enrichment in QueryResponseOps to
  // extract a list of case classes (or anything with a MapDecoder instance) from it.
  def query(client: Client, params: QueryParams): IO[QueryResponse] =
    Effect[IO].async((cb: Callback[QueryResponse]) => querier(client, params, cb))

  // Private helper methods.

  type Callback[A] = Either[Throwable, A] => Unit

  // Convert the tozny Result into an Either so it can be wrapped in IO.
  // The ErrorResult is kind of strange, IMO, because you get E3DBExceptions
  // from error(), and all other Throwables from other(). So, you need to
  // see if error() returns null - if it does, check other().
  private def resultToEither[A](result: Result[A]) =
    if (result.isError) {
      val error = result.asError().error()
      val exc   = if (error == null) result.asError().other() else error
      exc.asLeft
    } else result.asValue().asRight

  private def registrar(token: String,
                        clientName: String,
                        host: String,
                        callback: Callback[Config]): Unit =
    Client.register(token,
                    clientName,
                    host,
                    (result: Result[Config]) => callback(resultToEither(result)))

  private def writer(client: Client,
                     recordType: String,
                     data: JMap[String, String],
                     meta: JMap[String, String],
                     callback: Callback[Record]): Unit =
    client.write(recordType,
                 new RecordData(data),
                 meta,
                 (result: Result[Record]) => callback(resultToEither(result)))

  private def querier(client: Client,
                      params: QueryParams,
                      callback: Callback[QueryResponse]): Unit =
    client.query(params, (result: Result[QueryResponse]) => callback(resultToEither(result)))

}
