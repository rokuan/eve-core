package com.ideal.evecore.interpreter.remote


import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.interpreter._
import com.ideal.evecore.io.message.Result
import com.ideal.evecore.io.{StreamHandler, Serializers}
import com.ideal.evecore.io.command._


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(protected val id: String, protected val handler: StreamHandler) extends Context with QuerySource {
  implicit val formats = Serializers.buildRemoteFormats(handler, id)

  override def findItemsOfType(t: String): Option[EveObjectList] = handler.resultOperation[Result[EveObjectList]](FindItemsOfTypeCommand(t))

  /**
   * Queries the context to find a single item of a certain type
   *
   * @param t The type to query
   * @return A single object matching this type if some
   */
  override def findOneItemOfType(t: String): Option[EveStructuredObject] = handler.resultOperation[Result[EveStructuredObject]](FindOneItemOfTypeCommand(t))

  override def findById(id: String): Option[EveStructuredObject] = handler.resultOperation[Result[EveStructuredObject]](FindItemByIdCommand(id))

  implicit protected def getCommand(command: ContextCommand): UserCommand = ContextRequestCommand(id, command)
}
