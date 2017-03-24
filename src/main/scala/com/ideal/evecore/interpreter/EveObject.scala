package com.ideal.evecore.interpreter

import java.text.{SimpleDateFormat, DateFormat}
import java.util.Date

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.io.CommonKey
import com.rokuan.calliopecore.sentence.structure.content.{IPlaceObject, ITimeObject}

import scala.util.{Failure, Success, Try}

/**
 * Created by Christophe on 07/09/2016.
 */
object EveObject {
  val NumberResultType = classOf[EveNumberObject]
  val StringResultType = classOf[EveStringObject]
  val BooleanResultType = classOf[EveBooleanObject]
  val ObjectResultType = classOf[EveStructuredObject]
  val DateResultType = classOf[EveTimeObject]
  val PlaceResultType = classOf[EvePlaceObject]

  val TypeKey = "__eve_type"
  val ValueKey = "__eve_value"
  val IdKey = "__eve_id"

  val TextType = "text"
  val NumberType = "number"
  val DateType = "date"
  val BooleanType = "boolean"

  val fullDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z")

  implicit def stringToEveObject(s: String): EveObject = Try(fullDateFormat.parse(s)).map(EveDateObject).getOrElse(EveStringObject(s))
  implicit def doubleToEveObject(d: Double): EveNumberObject = EveNumberObject(d)
  implicit def numberToEveObject(n: Number): EveNumberObject = EveNumberObject(n)
  implicit def booleanToEveObject(b: Boolean): EveBooleanObject = EveBooleanObject(b)
  implicit def dateToEveObject(d: Date): EveDateObject = EveDateObject(d)
  implicit def arrayToEveObject(a: Array[EveObject]): EveObjectList = EveObjectList(a)
  implicit def mapToEveObject(m: Mapping[EveObject]): EveMappingObject = EveMappingObject(m)
  implicit def optionToEveObject(o: Option[_]): EveObject = o.map(apply).orNull

  implicit def eveStringObjectToEveStructuredObject(s: EveStringObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> TextType,
    CommonKey.Class -> StringResultType.getName,
    ValueKey -> s
  ))
  implicit def eveNumberObjectToEveStructuredObject(n: EveNumberObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> NumberType,
    CommonKey.Class -> NumberResultType.getName,
    ValueKey -> n
  ))
  implicit def eveDateObjectToEveStructuredObject(d: EveDateObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> DateType,
    CommonKey.Class -> DateResultType.getName,
    ValueKey -> d
  ))
  implicit def eveBooleanObjectToEveStructuredObject(b: EveBooleanObject): EveStructuredObject = EveMappingObject(Map(
    TypeKey -> BooleanType,
    CommonKey.Class -> BooleanResultType.getName,
    ValueKey -> b
  ))
  implicit def eveObjectToEveStructuredObject(o: EveObject): EveStructuredObject = o match {
    case o: EveStructuredObject => o
    case s: EveStringObject => s
    case n: EveNumberObject => n
    case b: EveBooleanObject => b
    case d: EveDateObject => d
  }

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
  def toFormattedString(): String = EveObject.fullDateFormat.format(d)
  override def toString() = DateFormat.getDateInstance(DateFormat.MEDIUM).format(d)
}
case class EveTimeObject(t: ITimeObject) extends EveObject
case class EvePlaceObject(p: IPlaceObject) extends EveObject
case class EveObjectList(a: Seq[EveObject]) extends EveObject {
  override def toString() = a.mkString(", ")
}

trait EveStructuredObject extends EveObject {
  def getType(): String
  def has(field: String): Boolean
  def hasState(state: String): Boolean
  def get(field: String): Option[EveObject]
  def getState(state: String): Option[String]
  def set(field: String, value: EveObject): Unit
  def setState(state: String, value: String): Unit
  def apply(field: String): EveObject = get(field).orNull
}

case class EveMappingObject(o: Mapping[EveObject]) extends EveStructuredObject {
  override def get(field: String): Option[EveObject] = o.get(field)
  override def set(field: String, value: EveObject): Unit = {}
  override def apply(field: String): EveObject = o(field)
  override def getState(state: String): Option[String] = Option.empty[String]
  override def setState(state: String, value: String): Unit = {}
  override def has(field: String): Boolean = o.contains(field)
  override def hasState(state: String): Boolean = false

  override def getType(): String = get(EveObject.TypeKey).collect { case s: EveStringObject => s.s }.getOrElse("")
}

case object NoneObject extends EveObject


trait EveResultObject

sealed case class EveSuccessObject(o: EveObject) extends EveResultObject
sealed case class EveFailureObject(error: String) extends EveResultObject {
  def this(e: Throwable) = this(e.getMessage)
}

object EveResultObject {
  implicit def tryToEveResultObject(t: Try[EveObject]): EveResultObject = t match {
    case Success(r) => EveSuccessObject(r)
    case Failure(t) => new EveFailureObject(t)
  }
  implicit def flattenTryToEveResultObject(t: Try[EveResultObject]): EveResultObject = t match {
    case Success(e) => e
    case Failure(f) => new EveFailureObject(f)
  }

  def Ok() = EveSuccessObject(NoneObject)
  def Ok(e: EveObject) = EveSuccessObject(e)
  def Ko(e: Throwable) = new EveFailureObject(e)
}

object EveObjectDSL {
  class EveObjectPath(val o: EveObject) {
    def \(field: String): EveObject = o.asInstanceOf[EveStructuredObject](field)
    def toNumber: Number = o.asInstanceOf[EveNumberObject].n
    def toBoolean: Boolean = o.asInstanceOf[EveBooleanObject].b
    def toText: String = o.asInstanceOf[EveStringObject].s
  }
  implicit def eveObjectToPath(o: EveObject) = new EveObjectPath(o)
}
