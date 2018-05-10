package example.e3dbs

import shapeless.ops.hlist
import shapeless.{HList, LabelledGeneric, Lazy}

// typeclass for splitting a Data instance into two Map[String, String]s, one for
// the encrypted data, and one for the metadata.
trait MetaSplitter[Data, Meta] {
  def apply(data: Data): (Map[String, String], Map[String, String])
}

object MetaSplitter {

  def apply[Data, Meta](implicit splitter: MetaSplitter[Data, Meta]): MetaSplitter[Data, Meta] =
    splitter

  implicit class MetaSplitterOps[Data](data: Data) {
    def splitMeta[Meta](implicit metaSplitter: MetaSplitter[Data, Meta])
      : (Map[String, String], Map[String, String]) =
      metaSplitter.apply(data)
  }

  implicit def genericMetaSplitter[Data,
                                   Meta,
                                   DataRepr <: HList,
                                   MetaRepr <: HList,
                                   NonMeta <: HList](
      implicit dataGen: LabelledGeneric.Aux[Data, DataRepr],
      metaGen: LabelledGeneric.Aux[Meta, MetaRepr],
      inter: hlist.Intersection.Aux[DataRepr, MetaRepr, MetaRepr],
      diff: hlist.Diff.Aux[DataRepr, MetaRepr, NonMeta],
      nonMetaEncoder: Lazy[MapEncoder[NonMeta]],
      metaEncode: Lazy[MapEncoder[MetaRepr]]): MetaSplitter[Data, Meta] = (data: Data) => {
    val dataRepr   = dataGen.to(data)
    val metaRepr   = inter(dataRepr)
    val leftRepr   = diff(dataRepr)
    val metaMap    = metaEncode.value.encode(metaRepr)
    val nonMetaMap = nonMetaEncoder.value.encode(leftRepr)
    (nonMetaMap, metaMap)
  }
}
