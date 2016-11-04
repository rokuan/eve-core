package com.ideal.evecore.interpreter

import com.ideal.evecore.universe.World
import com.ideal.evecore.universe.receiver.ObjectMessage
import com.ideal.evecore.universe.route.{NullValueSource, ValueSource, ObjectValueSource}
import com.rokuan.calliopecore.sentence.ActionObject
import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.structure.QuestionObject.QuestionType
import com.rokuan.calliopecore.sentence.structure.{OrderObject, AffirmationObject, QuestionObject, InterpretationObject}

import scala.util.{Success, Try}

/**
 * Created by Christophe on 20/09/2016.
 */
trait Evaluator[Q] {
  protected val context: Context[Q]
  protected val storage: Storage[Q]

  def eval(obj: InterpretationObject) = {
    obj match {
      case question: QuestionObject => evalQuestion(question)
      case affirmation: AffirmationObject => evalAffirmation(affirmation)
      case order: OrderObject => evalOrder(order)
    }
  }

  protected def evalQuestion(question: QuestionObject) = {
    import EveObject._

    val expectedType = question.questionType match {
      case QuestionType.HOW_MANY => NumberResultType
      case QuestionType.WHAT | QuestionType.WHO => classOf[EveObject]
      case QuestionType.WHEN => DateResultType
      case QuestionType.YES_NO => BooleanResultType
      case QuestionType.WHERE => PlaceResultType
      case _ => classOf[EveObject]
    }

    val verbStructure = question.getAction
    val action = verbStructure.getMainAction.getAction


    //if(action.does(ActionType.BE)){
    if(action == ActionType.BE){
      val result = storage.findObject(context, question.getDirectObject)
      // TODO: decommenter et verifier qu'il y a bien un resultat
      //result.map(v => v.asInstanceOf[expectedType.type])
      //result.get.asInstanceOf[expectedType.type].toString
      result.get.toString
    }
  }

  protected def evalAffirmation(affirmation: AffirmationObject) = {
    val verb: ActionObject = affirmation.getAction
    val action = verb.getMainAction

    // TODO: prendre en compte le temps du verbe (uniquement present pour l'instant)

    if(action.getAction == ActionType.BE){
      //database.update(context, affirmation.getSubject, affirmation.affirmation)
      //database.set(context, affirmation.getSubject, affirmation)
    } else if(action.getAction == ActionType.HAVE){
      //database.set(context, )
    } else if(action.isFieldBound) {
      val field = action.getBoundField
      storage.set(context, affirmation.getSubject, field, affirmation.getDirectObject)
    }
  }

  protected def evalOrder(order: OrderObject) = {
    import com.ideal.evecore.universe.route.ValueSourceConverters._

    val actionType = order.getAction.getMainAction.getAction

    val values = List[(String, Try[EveObject])](("subject", storage.findSubject(context, order.getSubject)),
    ("what", storage.findObject(context, order.getDirectObject)),
      ("when", storage.findTime(context, order.getTimeAdverbial)),
      ("how", storage.findWay(context, order.getWayAdverbial)),
      ("to", storage.findObject(context, order.getTarget)))

    val requestObject: ObjectValueSource = values.collect {
      case (k, Success(v)) => (k -> (v: ValueSource))
    }.foldLeft[Map[String, ValueSource]](Map[String, ValueSource]("action" -> actionType.name())){
      case (acc, p) => acc + p
    }

    World.findReceiver(requestObject).map { _.handleMessage(ObjectMessage(requestObject)) }


    /*val action: ActionObject = order.getAction

    if(action.does(ActionType.CONVERT)){
      val unit = order.getWayAdverbial match {
        case u: UnitObject => u.unitType
        case _ => throw new RuntimeException("Cannot convert to unspecified unit")
      }

      val quantityToTranslate = database.findObject(context, order.getDirectObject)
      quantityToTranslate.map(result => {
        result match {
          case EveStructuredObject(o) if o.getAs[String](EveDatabase.ClassKey).getOrElse("") == Writer.UnitObjectType.getName =>
            // TODO:
            new EveStructuredObject(null)
          case _ => throw new RuntimeException("Only units can be converted")
        }
      })
    } else if(action.does(ActionType.TRANSLATE)){
      val language = order.getWayAdverbial match {
        case l: LanguageObject => l.language.getLanguageCode
        case _ => Locale.getDefault.getLanguage
      }

      val textToTranslate = database.findObject(context, order.getDirectObject)
      textToTranslate.map(result => {
        result match {
          case EveStringObject(text) => new EveStringObject(Translator.translate(text, language))
          case EveStructuredObjectList(objects) =>
            val translations = objects.collect { case EveStringObject(s) => s }.map(text => new EveStringObject(Translator.translate(text, language)))
            new EveStructuredObjectList(translations)
        }
      })
    } else if(action.does(ActionType.SEND)){

    } else {
      // TODO:
      if(order.getTarget == null){
        val dest = database.findObject(context, order.getDirectObject, true)
        dest.map(target => target match {
          case EveStructuredObject(o) => {
            o.getAs[String](EveDatabase.CodeKey).map(code =>
              World.getReceiver(code).map(r =>
                r.handleMessage(ActionMessage(ActionType.TURN_OFF))
              )
            )
          }
          case EveStructuredObjectList(os) => {
            os.collect{ case EveStructuredObject(o) if o.get(EveDatabase.CodeKey).isDefined => o }
              .flatMap(o => World.getReceiver(o.getAs[String](EveDatabase.CodeKey).get))
              .map(r => r.handleMessage(ActionMessage(ActionType.TURN_OFF)))  // TODO: recuperer l'action principale
          }
        })
      } else {
        val what = database.findObject(context, order.getDirectObject, true)
        val to = database.findObject(context, order.getTarget, true)

        what.map(src =>
          to.map(target => target)
        )
      }
    }*/
  }
}
