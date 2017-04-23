package com.ideal.evecore.common

import com.ideal.evecore.util.{Result, Transformer, Option => EOpt}

import scala.util.{Failure, Success, Try}

/**
 * Created by Christophe on 07/03/17.
 */
object Conversions {
  implicit def tryToResult[T](t: Try[T]): Result[T] = t match {
    case Success(v) => Result.ok(v)
    case Failure(e) => Result.ko(e)
  }

  implicit def optionToTry[T](o: Option[T]): Try[T] = o.map(Success(_)).getOrElse(Failure(new Exception("No result")))

  implicit def toScalaOption[T](o: EOpt[T]): Option[T] =
    if (o.isEmpty) {
      Option.empty[T]
    } else {
      Some(o.get())
    }
  implicit def toEveOption[T](o: Option[T]): EOpt[T] = o.map(new EOpt.Some(_)).getOrElse(EOpt.empty())

  implicit def functionToTransformer[T, R](f: T => R) = new Transformer[T, R] {
    override def apply(t: T): R = f(t)
  }

  implicit def mapToMapping[T](m: Map[String, T]): Mapping[T] = new Mapping[T](m.map { case (key, value) => new com.ideal.evecore.util.Pair[String, T](key, value) }.toSeq: _*)
}
