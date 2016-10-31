package com.ideal.evecore.universe.route

import com.ideal.evecore.interpreter._
import com.ideal.evecore.universe.route.ValueSource.ObjectMap

sealed trait ValueSource {
  def isNumber(): Boolean = false
  def isString(): Boolean = false
  def isBoolean(): Boolean = false
  def isObject(): Boolean = false
  def isNull(): Boolean = false
  def isArray(): Boolean = false
  def getNumber(): Number = 0
  def getString(): String = ""
  def getBoolean(): Boolean = false
  def getObject(): ObjectMap = null
  def getValues(): Array[ValueSource] = Array()
}

case class NumberValueSource(n: Number) extends ValueSource {
  override final def isNumber(): Boolean = true
  override final def getNumber(): Number = n
}

case class BooleanValueSource(b: Boolean) extends ValueSource {
  override final def isBoolean(): Boolean = true
  override final def getBoolean(): Boolean = b
}

case class StringValueSource(s: String) extends ValueSource {
  override final def isString(): Boolean = true
  override final def getString(): String = s
}

case class ObjectValueSource(o: ObjectMap) extends ValueSource {
  def this(values: (String, ValueSource)*) = this(Map[String, ValueSource](values: _*))
  override final def isObject(): Boolean = true
  override final def getObject(): ObjectMap = o
}

case class ArrayValueSource(a: Array[ValueSource]) extends ValueSource {
  override def isArray(): Boolean = true
  override def getValues(): Array[ValueSource] = a
}

case object NullValueSource extends ValueSource {
  override final def isNull(): Boolean = true
}

object ValueSource {
  type ObjectMap = Map[String, ValueSource]

  implicit def stringToValueSource(s: String): StringValueSource = StringValueSource(s)
  implicit def numberToValueSource(n: Number): NumberValueSource = NumberValueSource(n)
  implicit def booleanToValueSource(b: Boolean): BooleanValueSource = BooleanValueSource(b)
  implicit def mapToObjectValueSource(m: Map[String, ValueSource]): ObjectValueSource = ObjectValueSource(m)
}

object ValueSourceConverters {
  implicit def eveObjectToValueSource(o: EveObject): ValueSource = o match {
    case EveStringObject(s) => StringValueSource(s)
    case EveNumberObject(n) => NumberValueSource(n)
    case EveBooleanObject(b) => BooleanValueSource(b)
    case EveStructuredObject(o) => ObjectValueSource(o.map { case (k, eo) => (k -> eveObjectToValueSource(eo)) }.toMap)
    case EveStructuredObjectList(os) => ArrayValueSource(os.map(eveObjectToValueSource(_)).toArray)
    case null => NullValueSource
  }

  implicit def eveStringObjectToStringValueSource(o: EveStringObject) = StringValueSource(o.s)
  implicit def eveNumberObjectToNumberValueSource(o: EveNumberObject) = NumberValueSource(o.n)
  implicit def eveBooleanObjectToBooleanValueSource(o: EveBooleanObject) = BooleanValueSource(o.b)
}
