package com.ideal.evecore.io


import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject


trait Writer[T] {
  def write(o: T): Mapping[EveObject]
}

object Writer {
  val LanguageObjectType = classOf[LanguageObject]
  val UnitObjectType = classOf[UnitObject]
  val QuantityObjectType = classOf[QuantityObject]
  val PhoneNumberObjectType = classOf[PhoneNumberObject]
  val ColorObjectType = classOf[ColorObject]
  val CityObjectType = classOf[CityObject]
  val CountryObjectType = classOf[CountryObject]
  val TransportObjectType = classOf[TransportObject]

  def write[T](o: T)(implicit w: Writer[T]) = w.write(o)

  implicit object LanguageObjectWriter extends Writer[LanguageObject] {
    override def write(o: LanguageObject) =
      Map(
        CommonKey.Class -> LanguageObjectType.getName,
        LanguageObjectKey.Code -> o.language.getLanguageCode
      )
  }

  implicit object UnitObjectWriter extends Writer[UnitObject] {
    override def write(o: UnitObject) =
      Map(
        CommonKey.Class -> UnitObjectType.getName,
        UnitObjectKey.Type -> o.unitType.name()
      )
  }

  implicit object QuantityObjectWriter extends Writer[QuantityObject] {
    override def write(o: QuantityObject) =
      Map(
        CommonKey.Class -> QuantityObjectType.getName,
        QuantityObjectKey.Value -> o.amount,
        QuantityObjectKey.Type -> o.unitType.name()
      )
  }

  implicit object PhoneNumberWriter extends Writer[PhoneNumberObject] {
    override def write(o: PhoneNumberObject) =
      Map(
        CommonKey.Class -> PhoneNumberObjectType.getName,
        PhoneNumberObjectKey.Value -> o.number
      )
  }

  implicit object TransportObjectWriter extends Writer[TransportObject] {
    override def write(o: TransportObject): Mapping[EveObject] =
      Map(
        CommonKey.Class -> TransportObjectType.getName,
        TransportObjectKey.Type -> o.transportType.name()
      )
  }
}