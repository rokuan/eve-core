package com.ideal.evecore.io

object CommonKey {
  val Class = "__class"
}

object InterpretationObjectKey {
  val Action = "action"
  val Subject = "subject"
  val What = "what"
  val How = "how"
  val Where = "where"
  val When = "when"
  val To = "to"
}

object NominalObjectKey {
  val GroupType = "group_type"
}

object WayObjectKey {
  val WayType = "way_type"
  val WayContext = "way_context"
}

object LanguageObjectKey {
  val Code = "language_code"
}

object UnitObjectKey {
  val Type = "unit_type"
}

object QuantityObjectKey {
  val Value = "unit_value"
  val Type = UnitObjectKey.Type
}

object PhoneNumberObjectKey {
  val Value = "phone_number_value"
}

object ColorObjectKey {
  val Code = "color_code"
}

object CityObjectKey {
  val Latitude = "city_latitude"
  val Longitude = "city_longitude"
}

object CountryObjectKey {
  val Code = "country_code"
}

object LocationObjectKey {
  val City = "location_city"
  val Country = "location_country"
}

object TransportObjectKey {
  val Type = "transport_type"
}