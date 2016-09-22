package com.ideal.evecore.universe

import com.ideal.evecore.universe.route.{ObjectValueSource, ValueSource}

/**
 * Created by Christophe on 14/09/2016.
 */
sealed trait ValueMatcher {
  def matches(v: ValueSource): Boolean
}

case class StringValueMatcher(s: String) extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = v.isString() && v.getString() == s
}

case class NumberValueMatcher(n: Number) extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = v.isNumber() && v.getNumber() == n
}

case object AnyValueMatcher extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = true
}

case class OrValueMatcher(a: Array[ValueMatcher]) extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = a.exists(_.matches(v))
}

case object NullValueMatcher extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = v.isNull()
}

case class ObjectValueMatcher(m: Map[String, ValueMatcher]) extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = v match {
    case ObjectValueSource(mappings) => m.forall { case (field, matcher) => mappings.contains(field) && matcher.matches(mappings(field)) }
    case _ => false
  }
}

case object UndefinedValueMatcher extends ValueMatcher {
  override def matches(v: ValueSource): Boolean = false
}

object ValueMatcher {
  implicit def arrayToValueMatcher[T](a: Array[T]): ValueMatcher = OrValueMatcher(a.map(apply(_)))
  implicit def stringToValueMatcher(s: String): ValueMatcher = s match {
    case "*" => AnyValueMatcher
    case _ => StringValueMatcher(s)
  }

  def apply(o: Any): ValueMatcher = o match {
    case s: String if s == "*" => AnyValueMatcher
    case s: String => StringValueMatcher(s)
    case a: Array[_] => arrayToValueMatcher(a)
    case n: Number => NumberValueMatcher(n)
    case m: Map[String, _] => ObjectValueMatcher(m.map { case (key, value) => (key -> apply(value)) })
    case null => NullValueMatcher
    case _ => UndefinedValueMatcher
  }
}