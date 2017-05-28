package com.ideal.evecore.interpreter

import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date

import com.ideal.evecore.interpreter.data._
import com.ideal.evecore.io.CommonKey
import com.rokuan.calliopecore.sentence.structure.content.{IPlaceObject, ITimeObject}

import EObject._
import com.ideal.evecore.common.Conversions._
import collection.JavaConversions._

import scala.util.Try

/**
 * Created by Christophe on 07/09/2016.
 */
object EObject {
  val NumberResultType = classOf[ENumberObject]
  val StringResultType = classOf[EStringObject]
  val BooleanResultType = classOf[EBooleanObject]
  val ObjectResultType = classOf[EStructuredObject]
  val DateResultType = classOf[ETimeObject]
  val PlaceResultType = classOf[EPlaceObject]

  val TypeKey = "__eve_type"
  val ValueKey = "__eve_value"
  val IdKey = "__eve_id"

  val TextType = "text"
  val NumberType = "number"
  val DateType = "date"
  val BooleanType = "boolean"

  val fullDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z")

  implicit def stringToEveObject(s: String): EObject = Try(fullDateFormat.parse(s)).map(EDateObject).getOrElse(EStringObject(s))
  implicit def doubleToEveObject(d: Double): ENumberObject = ENumberObject(d)
  implicit def numberToEveObject(n: Number): ENumberObject = ENumberObject(n)
  implicit def booleanToEveObject(b: Boolean): EBooleanObject = EBooleanObject(b)
  implicit def dateToEveObject(d: Date): EDateObject = EDateObject(d)
  implicit def arrayToEveObject(a: Array[EObject]): EObjectList = EObjectList(a.toList)
  //implicit def mapToEveObject(m: Mapping[EObject]): EMappingObject = EMappingObject(m)
  implicit def optionToEveObject(o: Option[_]): EObject = o.map(anyToEObject).orNull

  implicit def pairsToEveMappingObject(m: (String, EveObject)*) = new EveMappingObject(m.map(p => new com.ideal.evecore.util.Pair[String, EveObject](p._1, p._2)): _*)
  implicit def mappingToEveMappingObject(m: Map[String, EveObject]) = new EveMappingObject(m.map { case (key, value) => new com.ideal.evecore.util.Pair[String, EveObject](key, value) }.toSeq: _*)
  implicit def eMappingToEveMappingObject(m: Map[String, EObject]) = mappingToEveMappingObject(m.map { case (key, value) => (key -> implicitly[EveObject](value)) }.toMap)
  implicit def eMappingToEStructuredObject(m: Map[String, EObject]) = new EStructuredObject(eMappingToEveMappingObject(m))

  implicit def eveStringObjectToEveStructuredObject(s: EStringObject): EStructuredObject = Map[String, EObject](
    TypeKey -> TextType,
    CommonKey.Class -> StringResultType.getName,
    ValueKey -> s
  )
  implicit def eveNumberObjectToEveStructuredObject(n: ENumberObject): EStructuredObject = Map[String, EObject](
    TypeKey -> NumberType,
    CommonKey.Class -> NumberResultType.getName,
    ValueKey -> n
  )
  implicit def eveDateObjectToEveStructuredObject(d: EDateObject): EStructuredObject = Map[String, EObject](
    TypeKey -> DateType,
    CommonKey.Class -> DateResultType.getName,
    ValueKey -> d
  )
  implicit def eveBooleanObjectToEveStructuredObject(b: EBooleanObject): EStructuredObject = Map[String, EObject](
    TypeKey -> BooleanType,
    CommonKey.Class -> BooleanResultType.getName,
    ValueKey -> b
  )
  implicit def eveObjectToEveStructuredObject(o: EObject): EStructuredObject = o match {
    case o: EStructuredObject => o
    case s: EStringObject => s
    case n: ENumberObject => n
    case b: EBooleanObject => b
    case d: EDateObject => d
  }

  implicit def eveObjectListToEObjectList(o: EveObjectList): EObjectList = EObjectList(o.getValues.map(eveObjectToEObject).toList)

  implicit def eveStructuredObjectToEStructuredObject(o: EveStructuredObject): EStructuredObject = new EStructuredObject(o)
  implicit def eStructuredObjectToEveStructuredObject(o: EStructuredObject): EveStructuredObject = o.underlying

  implicit def anyToEObject(a: Any): EObject = a match {
    case null => null
    case s: String => s
    case d: Double => d
    case n: Number => n
    case b: Boolean => b
    case d: Date => d
    case a: Array[_] => EObjectList(a.map(anyToEObject).toList)
    case l: List[_] => EObjectList(l.map(anyToEObject))
    case m: Map[_, _] => EMappingObject(new EveMappingObject(m.map { case (k, o) => new com.ideal.evecore.util.Pair[String, EveObject](k.toString, anyToEveObject(o)) }.toSeq: _*))
    case o: Option[_] => o
    case o: EObject => o
  }

  implicit def anyToEveObject(a: Any): EveObject = a match {
    case null => null
    case s: String => new EveStringObject(s)
    case d: Double => new EveNumberObject(d)
    case n: Number => new EveNumberObject(n)
    case b: Boolean => new EveBooleanObject(b)
    case d: Date => new EveDateObject(d)
    case a: Array[_] => new EveObjectList(a.map(anyToEveObject): _*)
    case m: Map[_, _] => new EveMappingObject(m.map { case (k, o) => new com.ideal.evecore.util.Pair[String, EveObject](k.toString, anyToEveObject(o)) }.toSeq: _*)
    case o: EveObject => o
  }

  implicit def eveObjectToEObject(o: EveObject): EObject = o match {
    case s: EveStringObject => EStringObject(s.getValue)
    case b: EveBooleanObject => EBooleanObject(b.getValue)
    case n: EveNumberObject => ENumberObject(n.getValue)
    case d: EveDateObject => EDateObject(d.getValue)
    case q: EveQueryObject => new EQueryObject(q)
    case m: EveMappingObject => EMappingObject(m)
    case o: EveStructuredObject => new EStructuredObject(o)
    case l: EveObjectList => EObjectList(l.getValues.map(eveObjectToEObject(_)).toList)
  }

  implicit def eObjectToEveObject(o: EObject): EveObject = o match {
    case EStringObject(s) => new EveStringObject(s)
    case EBooleanObject(b) => new EveBooleanObject(b)
    case ENumberObject(n) => new EveNumberObject(n)
    case EDateObject(d) => new EveDateObject(d)
    //case EMappingObject(m) => m
    case o: EStructuredObject => o.underlying
    case EObjectList(l) => new EveObjectList(l.map(eObjectToEveObject(_)).toList)
  }
}

trait EObject

case class EBooleanObject(b: java.lang.Boolean) extends EObject {
  override def toString() = b.toString
}
case class ENumberObject(n: java.lang.Number) extends EObject {
  override def toString() = n.toString
}
case class EStringObject(s: String) extends EObject {
  override def toString() = s
}
case class EDateObject(d: Date) extends EObject {
  def toFormattedString(): String = EObject.fullDateFormat.format(d)
  override def toString() = DateFormat.getDateInstance(DateFormat.MEDIUM).format(d)
}
case class ETimeObject(t: ITimeObject) extends EObject
case class EPlaceObject(p: IPlaceObject) extends EObject
case class EObjectList(a: List[EObject]) extends EObject {
  def this(a: Array[EObject]) = this(a.toList)
  override def toString() = a.mkString(", ")
}

class EStructuredObject(val underlying: EveStructuredObject) extends EObject {
  def getType(): String = underlying.getType
  def has(field: String): Boolean = underlying.has(field)
  def hasState(state: String): Boolean = underlying.hasState(state)
  def get(field: String): Option[EObject] = implicitly[Option[EveObject]](underlying.get(field)).map(o => o: EObject)
  def getState(state: String): Option[String] = underlying.getState(state)
  def set(field: String, value: EObject): Boolean = underlying.set(field, value)
  def setState(state: String, value: String): Boolean = underlying.setState(state, value)
  def apply(field: String): EObject = get(field).orNull
}

case class EMappingObject(override val underlying: EveMappingObject) extends EStructuredObject(underlying) with EObject

/*case class EQueryMappingObject(override val underlying: EveQueryMappingObject) extends EStructuredObject(underlying) with EveQueryObject with EObject {
  override def getId: String = underlying.getId
}*/

case class EQueryObject(override val underlying: EveQueryObject) extends EStructuredObject(underlying) with EObject {
  def getId(): String = underlying.getId
}

object EveObjectDSL {
  implicit class EveObjectPath(o: EObject) {
    def \(field: String): EObject = o.asInstanceOf[EStructuredObject](field)
    def toNumber: Number = o.asInstanceOf[ENumberObject].n
    def toBoolean: Boolean = o.asInstanceOf[EBooleanObject].b
    def toText: String = o.asInstanceOf[EStringObject].s
    def toDate: Date = o.asInstanceOf[EDateObject].d
    def toList: List[EObject] = o.asInstanceOf[EObjectList].a
  }
}
