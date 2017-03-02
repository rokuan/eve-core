package com.ideal.evecore.io

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, IWayObject}
import com.rokuan.calliopecore.sentence.{ICityInfo, ICountryInfo}
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.LocationObject
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject


trait Writer[T] {
  def write(o: T): Mapping[EveObject]
}

object Writer {
  import com.ideal.evecore.interpreter.EveObject.TypeKey

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

  object NominalObjectWriter extends Writer[INominalObject] {
    override def write(o: INominalObject): Mapping[EveObject] =
      Map(NominalObjectKey.GroupType -> o.getGroupType.name())
  }

  object WayObjectWriter extends Writer[IWayObject]{
    override def write(o: IWayObject): Mapping[EveObject] =
      Map(
        WayObjectKey.WayType -> o.getWayType.name(),
        WayObjectKey.WayContext -> Option(o.getWayPreposition).map(_.getWayContext.name())
      )
  }

  implicit object LanguageObjectWriter extends Writer[LanguageObject] {
    override def write(o: LanguageObject) =
      WayObjectWriter.write(o) ++
        (Map(TypeKey -> "language",
          CommonKey.Class -> LanguageObjectType.getName,
          LanguageObjectKey.Code -> o.language.getLanguageCode
        ): Mapping[EveObject])
  }

  implicit object UnitObjectWriter extends Writer[UnitObject] {
    override def write(o: UnitObject) = {
      WayObjectWriter.write(o) ++
        (Map(
          TypeKey -> "unit",
          CommonKey.Class -> UnitObjectType.getName,
          UnitObjectKey.Type -> o.unitType.name()
        ): Mapping[EveObject])
    }
  }

  implicit object QuantityObjectWriter extends Writer[QuantityObject] {
    override def write(o: QuantityObject) =
      NominalObjectWriter.write(o) ++
        (Map(
          CommonKey.Class -> QuantityObjectType.getName,
          QuantityObjectKey.Value -> o.amount,
          QuantityObjectKey.Type -> o.unitType.name()
        ): Mapping[EveObject])
  }

  implicit object PhoneNumberWriter extends Writer[PhoneNumberObject] {
    override def write(o: PhoneNumberObject) =
      Map(
        TypeKey -> "phone_number",
        CommonKey.Class -> PhoneNumberObjectType.getName,
        PhoneNumberObjectKey.Value -> o.number
      )
  }

  implicit object TransportObjectWriter extends Writer[TransportObject] {
    override def write(o: TransportObject): Mapping[EveObject] =
      Map(
        TypeKey -> "transport",
        CommonKey.Class -> TransportObjectType.getName,
        TransportObjectKey.Type -> o.transportType.name()
      )
  }

  implicit object CountryWriter extends Writer[CountryObject] {
    override def write(o: CountryObject): Mapping[EveObject] =
      CountryInfoWriter.write(o.country) ++
        (Map(
          TypeKey -> "country",
          CommonKey.Class -> (CountryObjectType.getName: EveObject)
        ): Mapping[EveObject])
  }

  implicit object ColorWriter extends Writer[ColorObject] {
    override def write(o: ColorObject): Mapping[EveObject] =
      Map(
        TypeKey -> "color",
        CommonKey.Class -> ColorObjectType.getName,
        ColorObjectKey.Code -> o.color.getColorHexCode
      )
  }

  implicit object CityWriter extends Writer[CityObject] {
    override def write(o: CityObject): Mapping[EveObject] =
      CityInfoWriter.write(o.city) ++
        (Map(
          TypeKey -> "city",
          CommonKey.Class -> CityObjectType.getName
        ): Mapping[EveObject])
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