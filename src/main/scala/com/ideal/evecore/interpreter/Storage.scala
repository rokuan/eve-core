package com.ideal.evecore.interpreter

import com.ideal.evecore.io.Writer
import com.rokuan.calliopecore.sentence.IPronoun
import com.rokuan.calliopecore.sentence.structure.content.INominalObject
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.{PlaceObject, NamedPlaceObject, LocationObject, AdditionalPlace}
import com.rokuan.calliopecore.sentence.structure.data.time.SingleTimeObject

import scala.util.Try

/**
  * Created by Christophe on 15/07/2016.
  */
trait Storage[QueryType] {
  def update(context: Context[QueryType], left: INominalObject, value: INominalObject)
  def set(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def set(context: Context[QueryType], left: INominalObject, value: INominalObject)

  def get() = {}

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
      case language: LanguageObject => findLanguage(language)
      case location: LocationObject => null // TODO:
      case namedPlace: NamedPlaceObject => findNamedPlace(namedPlace)
      //case number:  =>
      case additionalObject: AdditionalObject => findAdditionalDataByCode(additionalObject.`object`.getCode)
      case additionalPerson: AdditionalPerson => findAdditionalDataByCode(additionalPerson.person.getCode)
      case phoneNumber: PhoneNumberObject => Try(new EveStructuredObject(Writer.write(phoneNumber)))
      case placeType: PlaceObject => null
      case pronounSubject: PronounSubject => resolvePronounSubject(context, pronounSubject)
      case quantity: QuantityObject => Try(new EveStructuredObject(Writer.write(quantity)))
      case unit: UnitObject => Try(new EveStructuredObject(Writer.write(unit)))
      case verbalGroup: VerbalGroup => null
      case _ => null
    }
  }

  protected def resolvePronounSubject(context: Context[QueryType], pronounSubject: PronounSubject): Try[EveObject] = findPronounSource(context, pronounSubject.pronoun)
  protected def findAbstractTarget(context: Context[QueryType], abstractTarget: AbstractTarget): Try[EveObject] = findPronounSource(context, abstractTarget.source)

  def findPronounSource(context: Context[QueryType], pronoun: IPronoun): Try[EveObject]
  //def findAbstractTarget(context: Context[QueryType], target: AbstractTarget): Try[EveObject]
  def findNameObject(context: Context[QueryType], name: NameObject): Try[EveObject]
  def findCharacter(context: Context[QueryType], char: CharacterObject): Try[EveObject]
  def findNamedPlace(place: NamedPlaceObject): Try[EveObject]
  def findLanguage(language: LanguageObject): Try[EveObject]
  def findColor(color: ColorObject): Try[EveObject]
  def findCity(city: CityObject): Try[EveObject]
  def findCountry(country: CountryObject): Try[EveObject]
  def findAdditionalDataByCode(code: String): Try[EveObject]
}
