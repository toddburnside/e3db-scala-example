package example.e3dbs

import cats.data.ValidatedNel
import cats.implicits._
import mouse.all._
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

sealed trait MapDecodeError
case class MissingField(fieldName: String) extends MapDecodeError
case class InvalidData(data: String)       extends MapDecodeError

// typeclass for decoding a Map[String, String] into a type A.
trait MapDecoder[A] {
  def decode(map: Map[String, String]): ValidatedNel[MapDecodeError, A]
}

object MapDecoder {
  def apply[A](implicit decoder: MapDecoder[A]): MapDecoder[A] = decoder

  def pure[A](f: Map[String, String] => ValidatedNel[MapDecodeError, A]): MapDecoder[A] =
    new MapDecoder[A] {
      override def decode(map: Map[String, String]): ValidatedNel[MapDecodeError, A] = f(map)
    }

  // instances for HLists.
  implicit def hnilDecoder: MapDecoder[HNil] = pure(_ => HNil.asRight.toValidatedNel)

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      read: Lazy[Read[H]],
      tDecoder: MapDecoder[T]): MapDecoder[FieldType[K, H] :: T] = {
    val fieldKey: Symbol = witness.value
    pure { map =>
      {
        val fieldStr: Either[MapDecodeError, String] =
          map
            .get(fieldKey.name)
            .map(_.asRight)
            .getOrElse(MissingField(fieldKey.name).asLeft)
        val fieldValue = fieldStr.flatMap(read.value.read(_)).map(field[K](_)).toValidatedNel
        (fieldValue, tDecoder.decode(map)).mapN(_ :: _)
      }
    }
  }

  // automatic derivation for case classes, etc.
  implicit def genericDecoder[A, ARepr <: HList](implicit lgen: LabelledGeneric.Aux[A, ARepr],
                                                 decoder: MapDecoder[ARepr]): MapDecoder[A] =
    pure[A] { map =>
      decoder.decode(map).map(lgen.from(_))
    }
}

// typeclass for decoding the string into a field of type A.
trait Read[A] {
  def read(s: String): Either[MapDecodeError, A]
}

object Read {
  // instances of Read. I'm only using strings and ints right now, but in a real
  // application, we'd need more.
  implicit def stringRead: Read[String] = (s: String) => s.asRight

  implicit def intRead: Read[Int] = (s: String) => s.parseInt.leftMap(_ => InvalidData(s))
}
