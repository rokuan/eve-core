package com.ideal.evecore.evaluator

import java.util.Calendar
import java.util.Date

import com.ideal.evecore.evaluator.Evaluator._
import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.data._
import com.ideal.evecore.io.Writer
import com.ideal.evecore.util.Result
import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, ITimeObject, IWayObject}
import com.rokuan.calliopecore.sentence.structure.data.count.CountObject.CountType
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.{AdditionalPlace, LocationObject, NamedPlaceObject, PlaceObject}
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.DateDefinition
import com.rokuan.calliopecore.sentence.structure.data.time.{DayPartObject, RelativeTimeObject, SingleTimeObject, TimePeriodObject}
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject
import com.rokuan.calliopecore.sentence.structure.{AffirmationObject, InterpretationObject, OrderObject, QuestionObject}
import com.rokuan.calliopecore.sentence.{ActionObject, IPronoun}

import EObject._
import com.ideal.evecore.common.Conversions._

import scala.util.{Failure, Success, Try}

/**
  * Created by Christophe on 20/09/2016.
  */
trait Interpreter extends Evaluator {
  protected val context: Context
  protected val taskHandler: TaskHandler
  protected val history: History

  override def eval(obj: InterpretationObject): Result[EveObject] = {
    obj match {
      case question: QuestionObject => evalQuestion(question)
      case affirmation: AffirmationObject => evalAffirmation(affirmation)
      case order: OrderObject => evalOrder(order)
    }
  }

  /**
   * Evaluate a question object
   * @param question The question to evaluate
   * @return The answer to this question as an EveObject instance
   */
  protected def evalQuestion(question: QuestionObject): Result[EveObject] = {

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
      findObject(question.getDirectObject).map(implicitly[EveObject](_))
    } else {
      EveResultObject.ok()
    }
  }

  /**
   * Evaluates an affirmative sentence object
   * @param affirmation
   * @return A succesful result
   */
  protected def evalAffirmation(affirmation: AffirmationObject): Result[EveObject] = {
    val verb: ActionObject = affirmation.getAction
    val action = verb.getMainAction

    // TODO: prendre en compte le temps du verbe (uniquement present pour l'instant)

    if(action.getAction == ActionType.BE){
      //affirmation.getDirectObject
      EveResultObject.ko(new Exception("Not implemented yet"))
    } else if(action.getAction == ActionType.HAVE){
      set(affirmation.getSubject, affirmation.getDirectObject)
      EveResultObject.ok()
    } else if(action.isFieldBound) {
      val field = action.getBoundField
      set(affirmation.getSubject, field, affirmation.getDirectObject)
      EveResultObject.ok()
    } else {
      EveResultObject.ok()
    }
  }

  /**
   * Executes an order
   * @param order The order to process
   * @return The result of this order if any
   */
  protected def evalOrder(order: OrderObject): Result[EveObject] = {
    import com.ideal.evecore.io.InterpretationObjectKey._

    val action = order.getAction.getMainAction
    val actionType = action.getAction
    val subject = findSubject(order.getSubject)
    val what = findObject(order.getDirectObject)
    val how = findWay(order.getWayAdverbial)
    val when = findTime(order.getTimeAdverbial)
    val target = findObject(order.getTarget)

    if(action.isStateBound){
      val result = for {
        time <- when
        target <- what
      } yield {
        taskHandler.scheduleDelayedStateTask(time, action, target)
      }
      result match {
        case Success(v) => v
        case Failure(e) => Result.ko(e)
      }
    } else {
      val values = List[(String, Try[EObject])](
        (Subject, subject),
        (What, what),
        (How, how),
        (To, target)
      )

      /*val requestObject: ObjectValueSource = values.collect { case (k, Success(v)) => (k -> (v: ValueSource)) }
      .foldLeft[Map[String, ValueSource]](Map[String, ValueSource](Action -> actionType.name())){ case (acc, p) => acc + p }

      when.map(taskHandler.scheduleDelayedTask(_, requestObject))*/
      val requestObject: EStructuredObject = values.collect {
        case (k, Success(v)) => (k -> v)
      }.foldLeft[Map[String, EObject]](Map[String, EObject](Action -> actionType.name())){ case (acc, p) => acc + p }
      when match {
        case Success(t) => taskHandler.scheduleDelayedTask(t, requestObject)
        case Failure(e) => Result.ko(e)
      }
    }
  }

  def findSubject(subject: INominalObject): Try[EObject] = findObject(subject)

  final def findObject(src: INominalObject, createIfNeeded: Boolean = false): Try[EObject] = {
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
      case person: PersonObject => Success(new EveStringObject(person.name))
      case _: VerbalGroup | _ => notImplementedYet
    }
  }

  def findTime(time: ITimeObject): Try[EObject] = {
    time match {
      case null => Success(EDateObject(new Date()))
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

          EDateObject(result.getTime)
        }
      }
      case r: RelativeTimeObject => Success(EDateObject(r.getDate))
      case d: DayPartObject => notImplementedYet
      case p: TimePeriodObject => Success(EObjectList(Array(EDateObject(p.getFrom), EDateObject(p.getTo))))
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

  def findWay(way: IWayObject): Try[EObject] = {
    way match {
      case null => Failure(new Exception("Source object is null"))
      case color: ColorObject => Try(Writer.write(color))
      case unit: UnitObject => Try(Writer.write(unit))
      case language: LanguageObject => Try(Writer.write(language))
      case transport: TransportObject => Try(Writer.write(transport))
      case _ => notImplementedYet
    }
  }

  final private def resolvePronounSubject(pronounSubject: PronounSubject): Try[EObject] = findPronounSource(pronounSubject.pronoun)
  final protected def findAbstractTarget(abstractTarget: AbstractTarget): Try[EObject] = findPronounSource(abstractTarget.source)

  def findPronounSource(pronoun: IPronoun): Try[EObject]
  def findNameObject(name: NameObject): Try[EObject] = {
    // TODO: query the context according to the quantity and/or position
    val typeTag = name.`object`.getNameTag
    val result: Option[EObject] =
      if(name.count.getType == CountType.ALL){
        context.findItemsOfType(typeTag).map((e: EveObjectList) => implicitly[EObjectList](e))
      } else {
        context.findOneItemOfType(typeTag).map((o: EveStructuredObject) => implicitly[EStructuredObject](o))
      }
    result
  }
  def findCharacter(char: CharacterObject): Try[EObject]
  def findNamedPlace(place: NamedPlaceObject): Try[EObject]
  def findAdditionalDataByCode(code: String): Try[EObject]

  def getCommonSuperTypes(os: List[EObject]): String

  private def set(source: EveObject, field: String, value: EveObject): Unit = {
    implicitly[EObject](source) match {
      case eso: EStructuredObject => eso.set(field, value)
      case eol: EObjectList => eol.a.collect {
        case eso: EStructuredObject => eso.set(field, value)
      }
      case _ =>
    }
  }

  def set(left: INominalObject, field: String, value: INominalObject): Unit = {
    for {
      source <- findObject(left)
      target <- findObject(value)
    } yield {
      set(source, field, target)
    }
  }

  def set(left: INominalObject, value: INominalObject): Unit = {
    for {
      source <- findObject(left).toOption
      target <- findObject(value).toOption
      field <- Option(target).collect {
        case eso: EStructuredObject => List(eso)
        case eol: EObjectList if eol.a.count(_.isInstanceOf[EveStructuredObject]) > 0 =>
          eol.a.filter(_.isInstanceOf[EveStructuredObject]).toList
      }.map(getCommonSuperTypes)
    } yield {
      set(source, field, target)
    }
  }

  def delete(left: INominalObject, field: String, value: INominalObject) = { /* TODO */ }
  def delete(left: INominalObject, value: INominalObject) = { /* TODO */ }
}

object Evaluator {
  def notImplementedYet = Failure(new Exception("Not implemented yet"))
}
