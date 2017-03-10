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
import EveObjectDSL._

import scala.util.Try

/**
 * Created by Christophe on 20/09/2016.
 */
class UnitConverterController extends Receiver {
  override def initReceiver(): Unit = {}

  override def getReceiverName(): String = getClass.getName

  override def getMappings(): Mapping[ValueMatcher] = Map(
    InterpretationObjectKey.Action -> StringValueMatcher(ActionType.CONVERT.name()),
    /*InterpretationObjectKey.What -> OrValueMatcher(
      ObjectValueMatcher(Map(NominalObjectKey.))
    ),*/
    InterpretationObjectKey.How -> ObjectValueMatcher(WayObjectKey.WayType -> StringValueMatcher(WayAdverbial.WayType.UNIT.name()))
  )

  override def handleMessage(message: Message): Try[EveObject] = message match {
    case EveObjectMessage(o) =>
      val srcValue = Try {
        val quantityObject = o \ InterpretationObjectKey.What
        (quantityObject \ QuantityObjectKey.Value toNumber, quantityObject \ QuantityObjectKey.Type toText)
      }.map { case (v, t) => (v, UnitType.valueOf(t)) }
      val destUnit = Try(o \ InterpretationObjectKey.How \ UnitObjectKey.Type toText).map(UnitType.valueOf)
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

  /**
   * Checks that the unit type is a Time unit type
   * @param unitType The unit type to test
   * @return true if it's a Time unit type, false otherwise
   */
  protected def isTimeUnitType(unitType: UnitType) = unitType match {
    case UnitType.MILLISECOND | UnitType.SECOND | UnitType.MINUTE | UnitType.HOUR | UnitType.DAY | UnitType.YEAR => true
    case _ => false
  }

  /**
   * Checks that the unit type is a Distance unit type
   * @param unitType The unit type to test
   * @return true if it's a Distance unit type, false otherwise
   */
  protected def isDistanceUnitType(unitType: UnitType) = unitType match {
    case UnitType.MILLIMETER | UnitType.CENTIMETER | UnitType.DECIMETER | UnitType.METER | UnitType.DECAMETER | UnitType.HECTOMETER | UnitType.KILOMETER => true
    case _ => false
  }

  /**
   * Converts a time between two time units
   * @param originalTime The original time (in fromTimeUnitType unit)
   * @param fromTimeUnitType The original unit
   * @param toTimeUnitType The destination unit
   * @return The converted time in toTimeUnitType unit
   */
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

  /**
   * Converts a distance between two distance units
   * @param originalDistance The original distance (in fromDistanceUnit unit)
   * @param fromDistanceUnit The original unit
   * @param toDistanceUnit The destination unit
   * @return The converted distance in toDistanceUnit unit
   */
  protected def distanceConverter(originalDistance: Number, fromDistanceUnit: UnitType, toDistanceUnit: UnitType): Number = {
    if(fromDistanceUnit == toDistanceUnit){
      originalDistance
    } else {
      originalDistance.doubleValue() * Math.pow(10, fromDistanceUnit.ordinal() - toDistanceUnit.ordinal())
    }
  }

  override def destroyReceiver(): Unit = {}
}
