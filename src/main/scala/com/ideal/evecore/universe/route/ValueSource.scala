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
    case EveMappingObject(o) => ObjectValueSource(o.map { case (k, eo) => (k -> eveObjectToValueSource(eo)) }.toMap)
    case EveObjectList(os) => ArrayValueSource(os.map(eveObjectToValueSource(_)).toArray)
    case null => NullValueSource
  }

  implicit def eveStringObjectToStringValueSource(o: EveStringObject) = StringValueSource(o.s)
  implicit def eveNumberObjectToNumberValueSource(o: EveNumberObject) = NumberValueSource(o.n)
  implicit def eveBooleanObjectToBooleanValueSource(o: EveBooleanObject) = BooleanValueSource(o.b)
}

object ValueSourceConversions {
  implicit def valueSourceToEveObject(v: ValueSource): EveObject = v match {
    case s: StringValueSource => s
    case n: NumberValueSource => n
    case b: BooleanValueSource => b
    case o: ObjectValueSource => o
    case NullValueSource => null
    case a: ArrayValueSource => a
  }

  implicit def stringValueSourceToEveStringObject(v: StringValueSource): EveStringObject = v.s
  implicit def numberValueSourceToEveNumberObject(n: NumberValueSource): EveNumberObject = n.n
  implicit def booleanValueSourceToEveBooleanObject(b: BooleanValueSource): EveBooleanObject = b.b
  implicit def objectValueSourceToEveStructuredObject(o: ObjectValueSource): EveMappingObject =
    EveMappingObject(o.o.map { case (k, v) => (k -> valueSourceToEveObject(v))})
  implicit def arrayValueSourceToEveObjectList(a: ArrayValueSource): EveObjectList =
    EveObjectList(a.a.map(valueSourceToEveObject))
}
