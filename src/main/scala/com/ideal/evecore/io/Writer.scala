package com.ideal.evecore.io

import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, IWayObject}
import com.rokuan.calliopecore.sentence.{ICityInfo, ICountryInfo}
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.LocationObject
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject
import com.ideal.evecore.interpreter.EObject
import com.ideal.evecore.interpreter.data.EveObject

trait Writer[T] {
  def write(o: T): Map[String, EObject]
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

  object NominalObjectWriter extends Writer[INominalObject] {
    override def write(o: INominalObject): Map[String, EObject] =
      Map[String, EObject](NominalObjectKey.GroupType -> o.getGroupType.name())
  }

  object WayObjectWriter extends Writer[IWayObject]{
    override def write(o: IWayObject): Map[String, EObject] =
      Map[String, EObject](
        WayObjectKey.WayType -> o.getWayType.name(),
        WayObjectKey.WayContext -> Option(o.getWayPreposition).map(_.getContext.name())
      )
  }

  implicit object LanguageObjectWriter extends Writer[LanguageObject] {
    override def write(o: LanguageObject) =
      WayObjectWriter.write(o) ++
        Map[String, EObject](
          EveObject.TYPE_KEY -> "language",
          CommonKey.Class -> LanguageObjectType.getName,
          LanguageObjectKey.Code -> o.language.getLanguageCode
        )
  }

  implicit object UnitObjectWriter extends Writer[UnitObject] {
    override def write(o: UnitObject) = {
      WayObjectWriter.write(o) ++
        Map[String, EObject](
          EveObject.TYPE_KEY -> "unit",
          CommonKey.Class -> UnitObjectType.getName,
          UnitObjectKey.Type -> o.unitType.name()
        )
    }
  }

  implicit object QuantityObjectWriter extends Writer[QuantityObject] {
    override def write(o: QuantityObject) =
      NominalObjectWriter.write(o) ++
        Map[String, EObject](
          CommonKey.Class -> QuantityObjectType.getName,
          QuantityObjectKey.Value -> o.amount,
          QuantityObjectKey.Type -> o.unitType.name()
        )
  }

  implicit object PhoneNumberWriter extends Writer[PhoneNumberObject] {
    override def write(o: PhoneNumberObject) =
      Map[String, EObject](
        EveObject.TYPE_KEY -> "phone_number",
        CommonKey.Class -> PhoneNumberObjectType.getName,
        PhoneNumberObjectKey.Value -> o.number
      )
  }

  implicit object TransportObjectWriter extends Writer[TransportObject] {
    override def write(o: TransportObject): Map[String, EObject] =
      Map[String, EObject](
        EveObject.TYPE_KEY -> "transport",
        CommonKey.Class -> TransportObjectType.getName,
        TransportObjectKey.Type -> o.transportType.name()
      )
  }

  implicit object CountryWriter extends Writer[CountryObject] {
    override def write(o: CountryObject): Map[String, EObject] =
      CountryInfoWriter.write(o.country) ++
        Map[String, EObject](
          EveObject.TYPE_KEY -> "country",
          CommonKey.Class -> CountryObjectType.getName
        )
  }

  implicit object ColorWriter extends Writer[ColorObject] {
    override def write(o: ColorObject): Map[String, EObject] =
      Map[String, EObject](
        EveObject.TYPE_KEY -> "color",
        CommonKey.Class -> ColorObjectType.getName,
        ColorObjectKey.Code -> o.color.getColorHexCode
      )
  }

  implicit object CityWriter extends Writer[CityObject] {
    override def write(o: CityObject): Map[String, EObject] =
      CityInfoWriter.write(o.city) ++
        Map[String, EObject](
          EveObject.TYPE_KEY -> "city",
          CommonKey.Class -> CityObjectType.getName
        )
  }

  object CityInfoWriter extends Writer[ICityInfo] {
    override def write(o: ICityInfo): Map[String, EObject] = {
      Map[String, EObject](
        CityObjectKey.Latitude -> o.getLocation.getLatitude,
        CityObjectKey.Longitude -> o.getLocation.getLongitude
      )
    }
  }

  object CountryInfoWriter extends Writer[ICountryInfo] {
    override def write(o: ICountryInfo): Map[String, EObject] = {
      Map[String, EObject](CountryObjectKey.Code -> o.getCountryCode)
    }
  }

  implicit object LocationWriter extends Writer[LocationObject] {
    override def write(o: LocationObject): Map[String, EObject] =
      Map[String, EObject](
        CommonKey.Class -> LocationObjectType.getName,
        LocationObjectKey.City -> CityInfoWriter.write(o.city),
        LocationObjectKey.Country -> CountryInfoWriter.write(o.country)
      )
  }
}