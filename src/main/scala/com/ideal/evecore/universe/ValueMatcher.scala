package com.ideal.evecore.universe

import com.ideal.evecore.interpreter._

/**
 * Created by Christophe on 14/09/2016.
 */
sealed trait ValueMatcher {
  /**
   * Checks that this matcher applies to an object
   * @param v The value to match
   * @return
   */
  def matches(v: EveObject): Boolean
}

case class StringValueMatcher(s: String) extends ValueMatcher {
  override def matches(v: EveObject): Boolean = v match {
    case EveStringObject(value) if value == s => true
    case _ => false
  }
}

case class NumberValueMatcher(n: Number) extends ValueMatcher {
  override def matches(v: EveObject): Boolean = v match {
    case EveNumberObject(value) if value == n => true
    case _ => false
  }
}

case object AnyValueMatcher extends ValueMatcher {
  override def matches(v: EveObject): Boolean = true
}

case class OrValueMatcher(a: Array[ValueMatcher]) extends ValueMatcher {
  override def matches(v: EveObject): Boolean = a.exists(_.matches(v))
}

case class BooleanValueMatcher(b: Boolean) extends ValueMatcher {
  override def matches(v: EveObject): Boolean = v match {
    case EveBooleanObject(value) if value == b => true
    case _ => false
  }
}

case object NullValueMatcher extends ValueMatcher {
  override def matches(v: EveObject): Boolean = v == null
}

//case class ObjectValueMatcher(m: Map[String, ValueMatcher]) extends ValueMatcher {
case class ObjectValueMatcher(m: (String, ValueMatcher)*) extends ValueMatcher {
  override def matches(v: EveObject): Boolean = v match {
    case o: EveStructuredObject => m.forall { case (field, matcher) => o.has(field) && matcher.matches(o(field)) }
    case _ => false
  }
}

case object UndefinedValueMatcher extends ValueMatcher {
  override def matches(v: EveObject): Boolean = false
}

object ValueMatcher {
  implicit def arrayToValueMatcher[T](a: Array[T]): ValueMatcher = OrValueMatcher(a.map(apply(_)))
  implicit def stringToValueMatcher(s: String): ValueMatcher = s match {
    case "*" => AnyValueMatcher
    case _ => StringValueMatcher(s)
  }
  implicit def booleanToValueMatcher(b: Boolean): ValueMatcher = BooleanValueMatcher(b)
  implicit def numberToValueMatcher(n: Number): ValueMatcher = NumberValueMatcher(n)

  def apply(o: Any): ValueMatcher = o match {
    case s: String if s == "*" => AnyValueMatcher
    case s: String => StringValueMatcher(s)
    case a: Array[_] => arrayToValueMatcher(a)
    case n: Number => NumberValueMatcher(n)
    case m: Map[_, _] => ObjectValueMatcher((m.map { case (key, value) => (key.toString -> apply(value)) }.toSeq): _*)
    case null => NullValueMatcher
    case _ => UndefinedValueMatcher
  }
}