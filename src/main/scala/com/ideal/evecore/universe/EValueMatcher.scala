package com.ideal.evecore.universe

import com.ideal.evecore.universe.matcher._

/**
 * Created by Christophe on 14/09/2016.
 */
sealed trait EValueMatcher
case class EStringValueMatcher(s: String) extends EValueMatcher
case class ENumberValueMatcher(n: Number) extends EValueMatcher
case object EAnyValueMatcher extends EValueMatcher
case class EOrValueMatcher(a: Array[EValueMatcher]) extends EValueMatcher
case class EBooleanValueMatcher(b: Boolean) extends EValueMatcher
case object ENullValueMatcher extends EValueMatcher
case class EObjectValueMatcher(m: Map[String, EValueMatcher]) extends EValueMatcher

object EObjectValueMatcher {
  def apply(m: (String, EValueMatcher)*): EObjectValueMatcher = EObjectValueMatcher(m.toMap)
}

case object EUndefinedValueMatcher extends EValueMatcher

object EValueMatcher {
  implicit def arrayToValueMatcher[T](a: Array[T]): ValueMatcher = new OrValueMatcher(a.map(apply(_)).toSeq: _*)
  implicit def stringToValueMatcher(s: String): ValueMatcher = s match {
    case "*" => new AnyValueMatcher
    case _ => new StringValueMatcher(s)
  }
  implicit def booleanToValueMatcher(b: Boolean): ValueMatcher = new BooleanValueMatcher(b)
  implicit def numberToValueMatcher(n: Number): ValueMatcher = new NumberValueMatcher(n)
  implicit def doubleToValueMatcher(d: Double): ValueMatcher =  new NumberValueMatcher(d)
  implicit def mapToValueMatcher(m: Map[String, ValueMatcher]) = new ObjectValueMatcher(m.map { case (key, value) => new com.ideal.evecore.util.Pair[String, ValueMatcher](key, value) }.toSeq: _*)
  implicit def seqToValueMatcher(vs: Seq[(String, ValueMatcher)]) = new ObjectValueMatcher(vs.map(p => new com.ideal.evecore.util.Pair[String, ValueMatcher](p._1, p._2)): _*)

  /*implicit def valueMatcherToEValueMatcher(v: ValueMatcher): EValueMatcher = v match {
    case b: BooleanValueMatcher => new
  }*/

  def apply(o: Any): ValueMatcher = o match {
    case s: String => s
    case b: Boolean => new BooleanValueMatcher(b)
    case a: Array[_] => arrayToValueMatcher(a)
    case n: Number => new NumberValueMatcher(n)
    case m: Map[_, _] => m.map { case (key, value) => (key.toString -> apply(value)) }
    case null => new NullValueMatcher
    case _ => new UndefinedValueMatcher
  }
}