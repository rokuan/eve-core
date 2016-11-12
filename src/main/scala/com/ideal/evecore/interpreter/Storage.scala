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

  def set(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def set(context: Context[QueryType], left: INominalObject, value: INominalObject)
  def delete(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def delete(context: Context[QueryType], left: INominalObject, value: INominalObject)

  def findSubject(context: Context[QueryType], subject: INominalObject): Try[EveObject] = findObject(context, subject)

  final def findObject(context: Context[QueryType], src: INominalObject, createIfNeeded: Boolean = false): Try[EveObject] = {
    src match {
      case null => Failure(new Exception("Source object is null"))
      case abstractTarget: AbstractTarget => findAbstractTarget(context, abstractTarget)
      case additionalPlace: AdditionalPlace => findAdditionalDataByCode(additionalPlace.place.getCode)
      case char: CharacterObject => findCharacter(context, char)
      case city: CityObject => Try(Writer.write(city))
      case color: ColorObject => Try(Writer.write(color))
      case name: NameObject => findNameObject(context, name)
      case country: CountryObject => Try(Writer.write(country))
      case date: SingleTimeObject => findTime(context, date)
      case language: LanguageObject => Try(Writer.write(language))
      case location: LocationObject => Try(Writer.write(location))
      case namedPlace: NamedPlaceObject => findNamedPlace(namedPlace)
      case additionalObject: AdditionalObject => findAdditionalDataByCode(additionalObject.`object`.getCode)
      case additionalPerson: AdditionalPerson => findAdditionalDataByCode(additionalPerson.person.getCode)
      case phoneNumber: PhoneNumberObject => Try(Writer.write(phoneNumber))
      case placeType: PlaceObject => null
      case pronounSubject: PronounSubject => resolvePronounSubject(context, pronounSubject)
      case quantity: QuantityObject => Try(Writer.write(quantity))
      case unit: UnitObject => Try(Writer.write(unit))
      case _: VerbalGroup | _ => notImplementedYet
    }
  }

  def findTime(context: Context[QueryType], time: ITimeObject): Try[EveObject] = {
    time match {
      case null => Failure(new Exception("Source object is null"))
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
      case null => Failure(new Exception("Source object is null"))
      case color: ColorObject => Try(Writer.write(color))
      case unit: UnitObject => Try(Writer.write(unit))
      case language: LanguageObject => Try(Writer.write(language))
      case transport: TransportObject => Try(Writer.write(transport))
      case _ => notImplementedYet
    }
  }

  final private def resolvePronounSubject(context: Context[QueryType], pronounSubject: PronounSubject): Try[EveObject] = findPronounSource(context, pronounSubject.pronoun)
  final protected def findAbstractTarget(context: Context[QueryType], abstractTarget: AbstractTarget): Try[EveObject] = findPronounSource(context, abstractTarget.source)

  def findPronounSource(context: Context[QueryType], pronoun: IPronoun): Try[EveObject]
  def findNameObject(context: Context[QueryType], name: NameObject): Try[EveObject]
  def findCharacter(context: Context[QueryType], char: CharacterObject): Try[EveObject]
  def findNamedPlace(place: NamedPlaceObject): Try[EveObject]
  def findAdditionalDataByCode(code: String): Try[EveObject]
}

object Storage {
  def notImplementedYet = Failure(new Exception("Not implemented yet"))
}