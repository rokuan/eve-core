package com.ideal.evecore.common

import com.ideal.evecore.io.message.Result

/**
 * Created by Christophe on 07/03/17.
 */
object Conversions {
  implicit def resultToOption[T >: Null](r: Result[T]) = r.success match {
    case true => Some(r.value)
    case _ => Option.empty[T]
  }

  implicit def optionToResult[T >: Null](o: Option[T]): Result[T] = o.map(Result(true, _)).getOrElse(Result(false, null))
}
