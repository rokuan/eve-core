package com.ideal.evecore.interpreter

import java.text.DateFormat
import java.util.Date

import com.ideal.evecore.common.Mapping.Mapping
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

  implicit def stringToEveObject(s: String): EveStringObject = EveStringObject(s)
  implicit def doubleToEveObject(d: Double): EveNumberObject = EveNumberObject(d)
  implicit def numberToEveObject(n: Number): EveNumberObject = EveNumberObject(n)
  implicit def booleanToEveObject(b: Boolean): EveBooleanObject = EveBooleanObject(b)
  implicit def dateToEveObject(d: Date): EveDateObject = EveDateObject(d)
  implicit def arrayToEveObject(a: Array[_]): EveObjectList = EveObjectList(a.map(apply))
  implicit def mapToEveObject(m: Map[_, _]): EveStructuredObject = EveStructuredObject(m.map { case (k, o) => (k.toString -> apply(o)) })

  def apply(a: Any): EveObject = a match {
    case null => null
    case s: String => s
    case d: Double => d
    case n: Number => n
    case b: Boolean => b
    case d: Date => d
    case a: Array[_] => a
    case m: Map[_, _] => m
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
case class EveStructuredObject(o: Mapping[EveObject]) extends EveObject
case class EveObjectList(a: Seq[EveObject]) extends EveObject {
  override def toString() = a.mkString(", ")
}