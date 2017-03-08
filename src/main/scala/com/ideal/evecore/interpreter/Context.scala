package com.ideal.evecore.interpreter


/**
  * Created by Christophe on 15/07/2016.
  */
trait Context {
  /**
   * Queries the context to get the items of a certain type
   * @param t The type
   * @return
   */
  def findItemsOfType(t: String): Option[EveObject]
}