package com.ideal.evecore.io.message


/**
 * Created by Christophe on 05/03/2017.
 */
case class Result[T >: Null](success: Boolean, value: T = null, error: String = "")

object Result {
  def Ok[T >: Null](v: T) = Result[T](true, v)
  def Ko[T >: Null](e: String) = Result[T](false, error = e)
  def Ko[T >: Null](t: Throwable) = Result[T](false, error = t.getStackTrace.map(_.toString).mkString("\n"))
}