package com.ideal.evecore.interpreter


/**
  * Created by Christophe on 15/07/2016.
  */
trait Context[QueryType] {
  def findItemsOfType(t: String): Option[EveObject]
}
