package com.ideal.evecore.universe.receiver.base

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter._
import com.ideal.evecore.io._
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Message, Receiver}
import com.ideal.evecore.universe.{ObjectValueMatcher, StringValueMatcher, ValueMatcher}
import com.rokuan.calliopecore.sentence.IAction.ActionType
import com.rokuan.calliopecore.sentence.structure.data.nominal.UnitObject.UnitType
import com.rokuan.calliopecore.sentence.structure.data.way.WayAdverbial
import com.rokuan.calliopecore.sentence.structure.data.nominal.QuantityObject

import scala.util.Try

/**
 * Created by Christophe on 20/09/2016.
 */
class UnitConverterController extends Receiver {
  override def initReceiver(): Unit = {}

  override def getReceiverName(): String = getClass.getName

  override def getMappings(): Mapping[_ <: ValueMatcher] = Map(
    InterpretationObjectKey.Action -> StringValueMatcher(ActionType.CONVERT.name()),
    /*InterpretationObjectKey.What -> OrValueMatcher(
      ObjectValueMatcher(Map(NominalObjectKey.))
    ),*/
    InterpretationObjectKey.How -> ObjectValueMatcher(Map(WayObjectKey.WayType -> WayAdverbial.WayType.UNIT.name()))
  )

  override def handleMessage(message: Message): Try[EveObject] = message match {
    case EveObjectMessage(o) =>
      val srcValue = Try {
        val quantityObject = o(InterpretationObjectKey.What).asInstanceOf[EveStructuredObject]
        (quantityObject(QuantityObjectKey.Value).asInstanceOf[EveNumberObject].n, quantityObject(QuantityObjectKey.Type).asInstanceOf[EveStringObject].s)
      }.map { case (v, t) => (v, UnitType.valueOf(t)) }
      val destUnit = Try {
        val unitObject = o(InterpretationObjectKey.How).asInstanceOf[EveStructuredObject]
        unitObject(UnitObjectKey.Type).asInstanceOf[EveStringObject].s
      }.map(UnitType.valueOf)
      for {
        (value, fromUnit) <- srcValue
        toUnit <- destUnit
      } yield {
        if(isTimeUnitType(fromUnit) && isTimeUnitType(toUnit)){
          val quantityObject = new QuantityObject
          quantityObject.amount = timeConverter(value, fromUnit, toUnit).doubleValue()
          quantityObject.unitType = toUnit
          EveMappingObject(Writer.write(quantityObject))
        } else if(isDistanceUnitType(fromUnit) && isDistanceUnitType(toUnit)) {
          val quantityObject = new QuantityObject
          quantityObject.amount = distanceConverter(value, fromUnit, toUnit).doubleValue()
          quantityObject.unitType = toUnit
          EveMappingObject(Writer.write(quantityObject))
        } else {
          throw new Exception("Unable to convert this value. Maybe those are two incompatible units")
        }
      }
    case _ => Try(throw new Exception("Unsupported operation"))
  }

  protected def isTimeUnitType(unitType: UnitType) = unitType match {
    case UnitType.MILLISECOND | UnitType.SECOND | UnitType.MINUTE | UnitType.HOUR | UnitType.DAY | UnitType.YEAR => true
    case _ => false
  }

  protected def isDistanceUnitType(unitType: UnitType) = unitType match {
    case UnitType.MILLIMETER | UnitType.CENTIMETER | UnitType.DECIMETER | UnitType.METER | UnitType.DECAMETER | UnitType.HECTOMETER | UnitType.KILOMETER => true
    case _ => false
  }

  protected def timeConverter(originalTime: Number, fromTimeUnitType: UnitType, toTimeUnitType: UnitType): Number = {
    if(fromTimeUnitType == toTimeUnitType){
      originalTime
    } else if(fromTimeUnitType.ordinal() < toTimeUnitType.ordinal()) {
      fromTimeUnitType match {
        case UnitType.MILLISECOND => timeConverter(originalTime.doubleValue() / 1000, UnitType.SECOND, toTimeUnitType)
        case UnitType.SECOND => timeConverter(originalTime.doubleValue() / 60, UnitType.MINUTE, toTimeUnitType)
        case UnitType.MINUTE => timeConverter(originalTime.doubleValue() / 60, UnitType.HOUR, toTimeUnitType)
        case UnitType.HOUR => timeConverter(originalTime.doubleValue() / 24, UnitType.DAY, toTimeUnitType)
        case UnitType.DAY => timeConverter(originalTime.doubleValue() / 365, UnitType.YEAR, toTimeUnitType)
        case _ => originalTime // TODO: Should not happen
      }
    } else {
      fromTimeUnitType match {
        case UnitType.YEAR => timeConverter(originalTime.longValue() * 365, UnitType.DAY, toTimeUnitType)
        case UnitType.DAY => timeConverter(originalTime.longValue() * 24, UnitType.HOUR, toTimeUnitType)
        case UnitType.HOUR => timeConverter(originalTime.longValue() * 60, UnitType.MINUTE, toTimeUnitType)
        case UnitType.MINUTE => timeConverter(originalTime.longValue() * 60, UnitType.SECOND, toTimeUnitType)
        case UnitType.SECOND => timeConverter(originalTime.longValue() * 1000, UnitType.MILLISECOND, toTimeUnitType)
        case _ => originalTime // TODO: Should not happen
      }
    }
  }

  protected def distanceConverter(originalDistance: Number, fromDistanceUnit: UnitType, toDistanceUnit: UnitType): Number = {
    if(fromDistanceUnit == toDistanceUnit){
      originalDistance
    } else {
      originalDistance.doubleValue() * Math.pow(10, fromDistanceUnit.ordinal() - toDistanceUnit.ordinal())
    }
  }

  override def destroyReceiver(): Unit = {}
}
