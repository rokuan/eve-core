package com.ideal.evecore.interpreter

import java.text.DateFormat
import java.util.Date

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.io.{CommonKey, Writer}
import com.rokuan.calliopecore.sentence.structure.content.{IPlaceObject, ITimeObject}

/**
 * Created by chris on 07/09/2016.
 */
object EveObject {
  val NumberResultType = classOf[EveNumberObject]
  val StringResultType = classOf[EveStringObject]
  val BooleanResultType = classOf[EveBooleanObject]
  val ObjectResultType = classOf[EveStructuredObject]
  val DateResultType = classOf[EveTimeObject]
  val PlaceResultType = classOf[EvePlaceObject]

  val TypeKey = "__eve_type"
  val Value = "__eve_value"

  implicit def stringToEveObject(s: String): EveStringObject = EveStringObject(s)
  implicit def doubleToEveObject(d: Double): EveNumberObject = EveNumberObject(d)
  implicit def numberToEveObject(n: Number): EveNumberObject = EveNumberObject(n)
  implicit def booleanToEveObject(b: Boolean): EveBooleanObject = EveBooleanObject(b)
  implicit def dateToEveObject(d: Date): EveDateObject = EveDateObject(d)
  implicit def arrayToEveObject(a: Array[EveObject]): EveObjectList = EveObjectList(a)
  implicit def mapToEveObject(m: Mapping[EveObject]): EveMappingObject = EveMappingObject(m)
  implicit def optionToEveObject(o: Option[_]): EveObject = o.map(apply).orNull

  implicit def eveStringObjectToEveStructuredObject(s: EveStringObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> "text",
    CommonKey.Class -> StringResultType.getName,
    Value -> s
  ))
  implicit def eveNumberObjectToEveStructuredObject(n: EveNumberObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> "number",
    CommonKey.Class -> NumberResultType.getName,
    Value -> n
  ))
  implicit def eveDateObjectToEveStructuredObject(d: EveDateObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> "date",
    CommonKey.Class -> DateResultType.getName,
    Value -> d
  ))
  implicit def eveBooleanObjectToEveStructuredObject(b: EveBooleanObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> "boolean",
    CommonKey.Class -> BooleanResultType.getName,
    Value -> b
  ))

  def apply(a: Any): EveObject = a match {
    case null => null
    case s: String => s
    case d: Double => d
    case n: Number => n
    case b: Boolean => b
    case d: Date => d
    case a: Array[_] => EveObjectList(a.map(apply))
    case m: Map[_, _] => EveMappingObject(m.map { case (k, o) => (k.toString -> apply(o)) })
    case o: Option[_] => o
    case o: EveObject => o
  }
}

trait EveObject

case class EveBooleanObject(b: java.lang.Boolean) extends EveObject {
  override def toString() = b.toString
}
case class EveNumberObject(n: java.lang.Number) extends EveObject {
  override def toString() = n.toString
}
case class EveStringObject(s: String) extends EveObject {
  override def toString() = s
}
case class EveDateObject(d: Date) extends EveObject {
  override def toString() = DateFormat.getDateInstance(DateFormat.MEDIUM).format(d)
}
case class EveTimeObject(t: ITimeObject) extends EveObject
case class EvePlaceObject(p: IPlaceObject) extends EveObject
case class EveObjectList(a: Seq[EveObject]) extends EveObject {
  override def toString() = a.mkString(", ")
}

trait EveStructuredObject extends EveObject {
  def getType(): String
  def get(field: String): Option[EveObject]
  def getState(state: String): Option[String]
  def set(field: String, value: EveObject): Unit
  def setState(state: String, value: String): Unit
  def apply(field: String): EveObject
}

case class EveMappingObject(o: Mapping[EveObject]) extends EveStructuredObject {
  override def get(field: String): Option[EveObject] = o.get(field)
  override def set(field: String, value: EveObject): Unit = {}
  override def apply(field: String): EveObject = o(field)
  override def getState(state: String): Option[String] = Option.empty[String]
  override def setState(state: String, value: String): Unit = {}

  override def getType(): String = get(EveObject.TypeKey).collect { case s: EveStringObject => s.s }.getOrElse("")
}

case object NoneObject extends EveObject