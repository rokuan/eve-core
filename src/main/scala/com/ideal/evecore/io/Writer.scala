package com.ideal.evecore.io


import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.rokuan.calliopecore.sentence.structure.data.nominal._


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

  def write[T](o: T)(implicit w: Writer[T]) = w.write(o)

  implicit object LanguageObjectWriter extends Writer[LanguageObject] {
    override def write(o: LanguageObject) =
      Set(
        CommonKey.Class -> LanguageObjectType.getName,
        LanguageObjectKey.Code -> o.language.getLanguageCode
      )
  }

  implicit object UnitObjectWriter extends Writer[UnitObject] {
    override def write(o: UnitObject) =
      Set(
        CommonKey.Class -> UnitObjectType.getName,
        UnitObjectKey.Type -> o.unitType.name()
      )
  }

  implicit object QuantityObjectWriter extends Writer[QuantityObject] {
    override def write(o: QuantityObject) =
      Set(
        CommonKey.Class -> QuantityObjectType.getName,
        QuantityObjectKey.Value -> o.amount,
        QuantityObjectKey.Type -> o.unitType.name()
      )
  }

  implicit object PhoneNumberWriter extends Writer[PhoneNumberObject] {
    override def write(o: PhoneNumberObject) =
      Set(
        CommonKey.Class -> PhoneNumberObjectType.getName,
        PhoneNumberObjectKey.Value -> o.number
      )
  }
}