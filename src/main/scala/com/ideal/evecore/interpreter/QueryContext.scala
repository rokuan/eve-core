package com.ideal.evecore.interpreter

/**
 * Created by chris on 07/03/17.
 */
trait QueryContext extends Context {
  def findById(id: String): Option[EveStructuredObject]
}
