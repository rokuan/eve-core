package com.ideal.evecore.interpreter

import java.util.Calendar

import com.ideal.evecore.io.Writer
import com.ideal.evecore.universe.execution.TaskHandler
import com.ideal.evecore.universe.route.{ObjectValueSource, ValueSource}
import com.rokuan.calliopecore.sentence.{ActionObject, IPronoun}
import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, ITimeObject, IWayObject}
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.{AdditionalPlace, LocationObject, NamedPlaceObject, PlaceObject}
import com.rokuan.calliopecore.sentence.structure.data.time.{DayPartObject, RelativeTimeObject, SingleTimeObject, TimePeriodObject}
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.DateDefinition
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject
import com.rokuan.calliopecore.sentence.structure.{AffirmationObject, InterpretationObject, OrderObject, QuestionObject}
import Evaluator._

import scala.util.{Failure, Success, Try}

/**
 * Created by Christophe on 20/09/2016.
 */
trait Evaluator[Q] {
  protected val context: Context[Q]
  protected val storage: Storage[Q]
  protected val taskHandler: TaskHandler

  def eval(obj: InterpretationObject): Try[EveObject] = {
    obj match {
      case question: QuestionObject => evalQuestion(question)
      case affirmation: AffirmationObject => evalAffirmation(affirmation)
      case order: OrderObject => evalOrder(order)
    }
  }

  protected def evalQuestion(question: QuestionObject): Try[EveObject] = {
    import EveObject._

    /*val expectedType = question.questionType match {
      case QuestionType.HOW_MANY => NumberResultType
      case QuestionType.WHAT | QuestionType.WHO => classOf[EveObject]
      case QuestionType.WHEN => DateResultType
      case QuestionType.YES_NO => BooleanResultType
      case QuestionType.WHERE => PlaceResultType
      case _ => classOf[EveObject]
    }*/

    val verbStructure = question.getAction
    val action = verbStructure.getMainAction.getAction

    if(action == ActionType.BE){
      findObject(question.getDirectObject)
    } else {
      Success(NoneObject)
    }
  }

  protected def evalAffirmation(affirmation: AffirmationObject): Try[EveObject] = {
    val verb: ActionObject = affirmation.getAction
    val action = verb.getMainAction

    // TODO: prendre en compte le temps du verbe (uniquement present pour l'instant)

    if(action.getAction == ActionType.BE){
      //affirmation.getDirectObject
      Failure(new Exception("Not implemented yet"))
    } else if(action.getAction == ActionType.HAVE){
      set(affirmation.getSubject, affirmation.getDirectObject)
      Success(NoneObject)
    } else if(action.isFieldBound) {
      val field = action.getBoundField
      set(affirmation.getSubject, field, affirmation.getDirectObject)
      Success(NoneObject)
    } else {
      Success(NoneObject)
    }
  }

  protected def evalOrder(order: OrderObject) = {
    import com.ideal.evecore.io.InterpretationObjectKey._
    import com.ideal.evecore.universe.route.ValueSourceConverters._

    val actionType = order.getAction.getMainAction.getAction
    val when = findTime(order.getTimeAdverbial)

    val values = List[(String, Try[EveObject])]((Subject, findSubject(order.getSubject)),
    (What, findObject(order.getDirectObject)),
      (How, findWay(order.getWayAdverbial)),
      (To, findObject(order.getTarget)))

    val requestObject: ObjectValueSource = values.collect {
      case (k, Success(v)) => (k -> (v: ValueSource))
    }.foldLeft[Map[String, ValueSource]](Map[String, ValueSource](Action -> actionType.name())){
      case (acc, p) => acc + p
    }

    when.map { time =>
      taskHandler.scheduleDelayedTask(time, requestObject)
      NoneObject
    }
  }

  def findSubject(subject: INominalObject): Try[EveObject] = findObject(subject)

  final def findObject(src: INominalObject, createIfNeeded: Boolean = false): Try[EveObject] = {
    src match {
      case null => Failure(new Exception("Source object is null"))
      case abstractTarget: AbstractTarget => findAbstractTarget(abstractTarget)
      case additionalPlace: AdditionalPlace => findAdditionalDataByCode(additionalPlace.place.getCode)
      case char: CharacterObject => findCharacter(char)
      case city: CityObject => Try(Writer.write(city))
      case color: ColorObject => Try(Writer.write(color))
      case name: NameObject => findNameObject(name)
      case country: CountryObject => Try(Writer.write(country))
      case date: SingleTimeObject => findTime(date)
      case language: LanguageObject => Try(Writer.write(language))
      case location: LocationObject => Try(Writer.write(location))
      case namedPlace: NamedPlaceObject => findNamedPlace(namedPlace)
      case additionalObject: AdditionalObject => findAdditionalDataByCode(additionalObject.`object`.getCode)
      case additionalPerson: AdditionalPerson => findAdditionalDataByCode(additionalPerson.person.getCode)
      case phoneNumber: PhoneNumberObject => Try(Writer.write(phoneNumber))
      case placeType: PlaceObject => null
      case pronounSubject: PronounSubject => resolvePronounSubject(pronounSubject)
      case quantity: QuantityObject => Try(Writer.write(quantity))
      case unit: UnitObject => Try(Writer.write(unit))
      case person: PersonObject => Success(EveStringObject(person.name))
      case _: VerbalGroup | _ => notImplementedYet
    }
  }

  def findTime(time: ITimeObject): Try[EveObject] = {
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

  def findWay(way: IWayObject): Try[EveObject] = {
    way match {
      case null => Failure(new Exception("Source object is null"))
      case color: ColorObject => Try(Writer.write(color))
      case unit: UnitObject => Try(Writer.write(unit))
      case language: LanguageObject => Try(Writer.write(language))
      case transport: TransportObject => Try(Writer.write(transport))
      case _ => notImplementedYet
    }
  }

  final private def resolvePronounSubject(pronounSubject: PronounSubject): Try[EveObject] = findPronounSource(pronounSubject.pronoun)
  final protected def findAbstractTarget(abstractTarget: AbstractTarget): Try[EveObject] = findPronounSource(abstractTarget.source)

  def findPronounSource(pronoun: IPronoun): Try[EveObject]
  def findNameObject(name: NameObject): Try[EveObject]
  def findCharacter(char: CharacterObject): Try[EveObject]
  def findNamedPlace(place: NamedPlaceObject): Try[EveObject]
  def findAdditionalDataByCode(code: String): Try[EveObject]

  def set(left: INominalObject, field: String, value: INominalObject) = { /* TODO */ }
  def set(left: INominalObject, value: INominalObject) = { /* TODO */ }
  def delete(left: INominalObject, field: String, value: INominalObject) = { /* TODO */ }
  def delete(left: INominalObject, value: INominalObject) = { /* TODO */ }
}

object Evaluator {
  def notImplementedYet = Failure(new Exception("Not implemented yet"))
}
