package com.ideal.evecore.interpreter


/**
  * Created by Christophe on 15/07/2016.
  */
trait Context {
  /**
   * Queries the context to get the items of a certain type
   * @param t The type to query
   * @return The list of items matching this type, if some
   */
  def findItemsOfType(t: String): Option[EveObjectList]

  /**
   * Queries the context to find a single item of a certain type
   * @param t The type to query
   * @return A single object matching this type if some
   */
  def findOneItemOfType(t: String): Option[EveStructuredObject]
}