package com.ideal.evecore.io

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.rokuan.calliopecore.sentence.{ICityInfo, ICountryInfo}
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.LocationObject
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
  val LocationObjectType = classOf[LocationObject]
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

  implicit object CountryWriter extends Writer[CountryObject] {
    override def write(o: CountryObject): Mapping[EveObject] =
      Map(
        CommonKey.Class -> (CountryObjectType.getName: EveObject)
      ) ++ CountryInfoWriter.write(o.country)
  }

  implicit object ColorWriter extends Writer[ColorObject] {
    override def write(o: ColorObject): Mapping[EveObject] =
      Map(
        CommonKey.Class -> ColorObjectType.getName,
        ColorObjectKey.Code -> o.color.getColorHexCode
      )
  }

  implicit object CityWriter extends Writer[CityObject] {
    override def write(o: CityObject): Mapping[EveObject] =
      Map(
        CommonKey.Class -> (CityObjectType.getName: EveObject)
      ) ++ CityInfoWriter.write(o.city)
  }

  object CityInfoWriter extends Writer[ICityInfo] {
    override def write(o: ICityInfo): Mapping[EveObject] = {
      Map(
        CityObjectKey.Latitude -> o.getLocation.getLatitude,
        CityObjectKey.Longitude -> o.getLocation.getLongitude
      )
    }
  }

  object CountryInfoWriter extends Writer[ICountryInfo] {
    override def write(o: ICountryInfo): Mapping[EveObject] = {
      Map(CountryObjectKey.Code -> o.getCountryCode)
    }
  }

  implicit object LocationWriter extends Writer[LocationObject] {
    override def write(o: LocationObject): Mapping[EveObject] =
      Map(
        CommonKey.Class -> LocationObjectType.getName,
        LocationObjectKey.City -> CityInfoWriter.write(o.city),
        LocationObjectKey.Country -> CountryInfoWriter.write(o.country)
      )
  }
}