package com.ideal.evecore.interpreter


import scala.collection.mutable

/**
 * Created by Christophe on 08/03/17.
 */
class Environment extends Context {
  private val contexts: mutable.ListBuffer[Context] = new mutable.ListBuffer[Context]()

  override def findItemsOfType(t: String): Option[EveObject] = contexts.synchronized {
    val results = contexts.map(_.findItemsOfType(t)).flatten
    val flattenedResults = results.foldLeft(List[EveObject]()){
      case (acc, o: EveObjectList) => o.a.toList ++ acc
      case (acc, o) => o :: acc
    }
    if(flattenedResults.size > 1){
      Some(EveObjectList(flattenedResults))
    } else {
      flattenedResults.headOption
    }
  }

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
