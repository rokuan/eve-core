package com.ideal.evecore.interpreter

import java.util.Calendar

import com.ideal.evecore.io.Writer
import com.rokuan.calliopecore.sentence.IPronoun
import com.rokuan.calliopecore.sentence.structure.content.{ITimeObject, IWayObject, INominalObject}
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.{PlaceObject, NamedPlaceObject, LocationObject, AdditionalPlace}
import com.rokuan.calliopecore.sentence.structure.data.time.{TimePeriodObject, DayPartObject, RelativeTimeObject, SingleTimeObject}
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.DateDefinition
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject

import scala.util.{Failure, Success, Try}

/**
  * Created by Christophe on 15/07/2016.
  */
trait Storage[QueryType] {
  import Storage._

  def update(context: Context[QueryType], left: INominalObject, value: INominalObject)
  def set(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def set(context: Context[QueryType], left: INominalObject, value: INominalObject)
  def delete(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def delete(context: Context[QueryType], left: INominalObject, value: INominalObject)

  def get() = {}

  def findSubject(context: Context[QueryType], subject: INominalObject): Try[EveObject]

  final def findObject(context: Context[QueryType], src: INominalObject, createIfNeeded: Boolean = false): Try[EveObject] = {
    src match {
      case abstractTarget: AbstractTarget => findAbstractTarget(context, abstractTarget)
      case additionalPlace: AdditionalPlace => findAdditionalDataByCode(additionalPlace.place.getCode)
      case char: CharacterObject => findCharacter(context, char)
      case city: CityObject => findCity(city)
      case color: ColorObject => findColor(color)
      case name: NameObject => findNameObject(context, name)
      case country: CountryObject => findCountry(country)
      case date: SingleTimeObject => Try(new EveTimeObject(date)) // TODO: voir quel type renvoyer (ITimeObject/Date)
      case language: LanguageObject => Try(new EveStructuredObject(Writer.write(language)))
      case location: LocationObject => findLocation(location)
      case namedPlace: NamedPlaceObject => findNamedPlace(namedPlace)
      case additionalObject: AdditionalObject => findAdditionalDataByCode(additionalObject.`object`.getCode)
      case additionalPerson: AdditionalPerson => findAdditionalDataByCode(additionalPerson.person.getCode)
      case phoneNumber: PhoneNumberObject => Try(new EveStructuredObject(Writer.write(phoneNumber)))
      case placeType: PlaceObject => null
      case pronounSubject: PronounSubject => resolvePronounSubject(context, pronounSubject)
      case quantity: QuantityObject => Try(new EveStructuredObject(Writer.write(quantity)))
      case unit: UnitObject => Try(new EveStructuredObject(Writer.write(unit)))
      case _: VerbalGroup | _ => notImplementedYet
    }
  }

  def findTime(context: Context[QueryType], time: ITimeObject): Try[EveObject] = {
    time match {
      case s: SingleTimeObject => {
        Try {
          val result = Calendar.getInstance()
          val date = Calendar.getInstance()
          date.setTime(s.date)

          if (s.dateDefinition == DateDefinition.DATE_AND_TIME || s.dateDefinition == DateDefinition.DATE_ONLY) {
            applyDate(date, result)
          }
          if (s.dateDefinition == DateDefinition.DATE_AND_TIME || s.dateDefinition == DateDefinition.TIME_ONLY) {
            applyTime(date, result)
          }

          EveDateObject(result.getTime)
        }
      }
      case r: RelativeTimeObject => Success(EveDateObject(r.getDate))
      case d: DayPartObject => notImplementedYet
      case p: TimePeriodObject => Success(EveObjectList(Array(EveDateObject(p.getFrom), EveDateObject(p.getTo))))
      case _ => notImplementedYet
    }
  }

  private def applyDate(source: Calendar, target: Calendar) = {
    target.set(Calendar.HOUR, 0)
    target.set(Calendar.MINUTE, 0)
    target.set(Calendar.SECOND, 0)
    target.set(Calendar.DATE, source.get(Calendar.DATE))
    target.set(Calendar.MONTH, source.get(Calendar.MONTH))
    target.set(Calendar.YEAR, source.get(Calendar.YEAR))
  }

  private def applyTime(source: Calendar, target: Calendar) = {
    target.set(Calendar.HOUR, source.get(Calendar.HOUR))
    target.set(Calendar.MINUTE, source.get(Calendar.MINUTE))
    target.set(Calendar.SECOND, source.get(Calendar.SECOND))
  }

  def findWay(context: Context[QueryType], way: IWayObject): Try[EveObject] = {
    way match {
      case color: ColorObject => findColor(color)
      case unit: UnitObject => Try(new EveStructuredObject(Writer.write(unit)))
      case language: LanguageObject => Try(new EveStructuredObject(Writer.write(language)))
      case transport: TransportObject => Try(new EveStructuredObject(Writer.write(transport)))
      case _ => notImplementedYet
    }
  }

  protected def resolvePronounSubject(context: Context[QueryType], pronounSubject: PronounSubject): Try[EveObject] = findPronounSource(context, pronounSubject.pronoun)
  protected def findAbstractTarget(context: Context[QueryType], abstractTarget: AbstractTarget): Try[EveObject] = findPronounSource(context, abstractTarget.source)

  def findPronounSource(context: Context[QueryType], pronoun: IPronoun): Try[EveObject]
  def findNameObject(context: Context[QueryType], name: NameObject): Try[EveObject]
  def findCharacter(context: Context[QueryType], char: CharacterObject): Try[EveObject]
  def findNamedPlace(place: NamedPlaceObject): Try[EveObject]
  def findLocation(location: LocationObject): Try[EveObject]
  def findColor(color: ColorObject): Try[EveObject]
  def findCity(city: CityObject): Try[EveObject]
  def findCountry(country: CountryObject): Try[EveObject]
  def findAdditionalDataByCode(code: String): Try[EveObject]
}

object Storage {
  def notImplementedYet = Failure(new Exception("Not implemented yet"))
}