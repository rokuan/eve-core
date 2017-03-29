package com.ideal.evecore.common

import com.ideal.evecore.interpreter.{EveFailureObject, EveSuccessObject, EveObject, EveResultObject}
import com.ideal.evecore.io.message.Result

import scala.util.{Failure, Success, Try}

/**
 * Created by Christophe on 07/03/17.
 */
object Conversions {
  implicit def resultToOption[T >: Null](r: Result[T]) = r.success match {
    case true => Some(r.value)
    case _ => Option.empty[T]
  }

  implicit def optionToResult[T >: Null](o: Option[T]): Result[T] = o.map(Result(true, _)).getOrElse(Result(false, null))

  implicit def tryToResult[T >: Null](t: Try[T]): Result[T] = t match {
    case Success(v) => Result.Ok(v)
    case Failure(e) => Result.Ko(e.getMessage)
  }

  implicit def resultToTry[T >: Null](r: Result[T]): Try[T] = r.success match {
    case true => Success(r.value)
    case _ => Failure(new Exception(r.error))
  }

  implicit def eveResultToResult(r: EveResultObject): Result[EveObject] = r match {
    case EveSuccessObject(v) => Result.Ok(v)
    case EveFailureObject(e) => Result.Ko(e)
    case _ => Result.Ko("Undefined result")
  }
}
