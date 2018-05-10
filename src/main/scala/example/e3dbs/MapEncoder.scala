package example.e3dbs

import cats._
import shapeless._
import shapeless.labelled.FieldType

trait MapEncoder[A] {
  def encode(value: A): Map[String, String]
}

object MapEncoder {
  def apply[A](implicit encoder: MapEncoder[A]): MapEncoder[A] = encoder

  def pure[A](f: A => Map[String, String]): MapEncoder[A] = new MapEncoder[A] {
    override def encode(value: A): Map[String, String] = f(value)
  }

  // instances for HLists.
  implicit val hnilEncoder: MapEncoder[HNil] = pure(_ => Map[String, String]())

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      show: Lazy[Show[H]],
      tEncoder: MapEncoder[T]): MapEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    pure { hlist =>
      val head = show.value.show(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      Map(fieldName -> head) ++ tail
    }
  }

  // automatic derivation for case classes, etc.
  implicit def genericEncoder[A, H <: HList](implicit generic: LabelledGeneric.Aux[A, H],
                                             hEncoder: Lazy[MapEncoder[H]]): MapEncoder[A] =
    pure { value =>
      hEncoder.value.encode(generic.to(value))
    }
}
