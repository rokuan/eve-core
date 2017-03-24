package com.ideal.evecore.interpreter


import scala.collection.mutable

/**
 * Created by Christophe on 08/03/17.
 */
class Environment extends Context {
  private val contexts: mutable.ListBuffer[Context] = new mutable.ListBuffer[Context]()

  override def findItemsOfType(t: String): Option[EveObjectList] = contexts.synchronized {
    val results = contexts.map(_.findItemsOfType(t)).flatten
    val flattenedResults = results.foldLeft(List[EveObject]()){
      case (acc, o: EveObjectList) => o.a.toList ++ acc
      case (acc, o) => o :: acc
    }
    if(flattenedResults.isEmpty){
      None
    } else {
      Some(EveObjectList(flattenedResults))
    }
  }

  /**
   * Queries the context to find a single item of a certain type
   * @param t The type to query
   * @return A single object matching this type if some
   */
  override def findOneItemOfType(t: String): Option[EveStructuredObject] = contexts.synchronized(contexts.toStream.flatMap(_.findOneItemOfType(t)).headOption)

  /**
   * Adds a context to this environment of contexts
   * @param context
   */
  def addContext(context: Context): Unit = contexts.synchronized(contexts += context)

  /**
   * Removes a context from this environment
   * @param context
   */
  def removeContext(context: Context): Unit = contexts.synchronized(contexts -= context)
}
