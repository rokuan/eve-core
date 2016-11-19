package com.ideal.evecore.interpreter

import com.ideal.evecore.universe.World
import com.ideal.evecore.universe.receiver.ObjectMessage
import com.ideal.evecore.universe.route.{ObjectValueSource, ValueSource}
import com.rokuan.calliopecore.sentence.ActionObject
import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.structure.{AffirmationObject, InterpretationObject, OrderObject, QuestionObject}

import scala.util.{Failure, Success, Try}

/**
 * Created by Christophe on 20/09/2016.
 */
trait Evaluator[Q] {
  protected val context: Context[Q]
  protected val storage: Storage[Q]

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
      storage.findObject(context, question.getDirectObject)
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
      storage.set(context, affirmation.getSubject, affirmation.getDirectObject)
      Success(NoneObject)
    } else if(action.isFieldBound) {
      val field = action.getBoundField
      storage.set(context, affirmation.getSubject, field, affirmation.getDirectObject)
      Success(NoneObject)
    } else {
      Success(NoneObject)
    }
  }

  protected def evalOrder(order: OrderObject) = {
    import com.ideal.evecore.io.InterpretationObjectKey._
    import com.ideal.evecore.universe.route.ValueSourceConverters._

    val actionType = order.getAction.getMainAction.getAction

    val values = List[(String, Try[EveObject])]((Subject, storage.findSubject(context, order.getSubject)),
    (What, storage.findObject(context, order.getDirectObject)),
      (When, storage.findTime(context, order.getTimeAdverbial)),
      (How, storage.findWay(context, order.getWayAdverbial)),
      (To, storage.findObject(context, order.getTarget)))

    val requestObject: ObjectValueSource = values.collect {
      case (k, Success(v)) => (k -> (v: ValueSource))
    }.foldLeft[Map[String, ValueSource]](Map[String, ValueSource](Action -> actionType.name())){
      case (acc, p) => acc + p
    }

    World.findReceiver(requestObject)
      .map { _.handleMessage(ObjectMessage(requestObject)) }
      .getOrElse(Failure(new Exception("Can't find a receiver to handle this action")))
  }
}
