package example.e3dbs

import scala.collection.JavaConverters._

import cats.data.ValidatedNel
import com.tozny.e3db.QueryResponse

// Allow the "extraction" of a list of case classes from a tozny QueryResponse.
trait QueryResponseOps {
  val queryResponse: QueryResponse

  def extract[A](implicit decoder: MapDecoder[A]): List[ValidatedNel[MapDecodeError, A]] =
    for {
      record <- queryResponse.records().asScala.toList
      map = (record.data().asScala ++ record.meta().plain().asScala).toMap
    } yield decoder.decode(map)
}

object QueryResponseOps {
  implicit def toQueryReponseOps(qr: QueryResponse): QueryResponseOps = new QueryResponseOps {
    override val queryResponse: QueryResponse = qr
  }
}
