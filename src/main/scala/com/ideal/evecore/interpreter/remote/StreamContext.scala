package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{EveStructuredObject, QueryContext, EveObject, Context}

import scala.util.Try
import scala.util.control.Breaks
import RemoteEveStructuredObjectMessage._
import com.ideal.evecore.io.Readers._

/**
 * Created by chris on 07/03/17.
 */
class StreamContext(protected val socket: Socket, val context: QueryContext) extends Context with StreamUtils {
  readEndlessly()

  private final def readEndlessly() = Try {
    val breaks = new Breaks

    breaks.breakable {
      while(true){
        safe {
          Option(readCommand()).map { command =>
            command match {
              case RemoteContextMessage.FindItemsOfType => {
                val queryType = readValue()
                val result = findItemsOfType(queryType)
                writeResultValue(result)
              }
              case RemoteContextMessage.ObjectRequest => {
                val objectId = readValue()
                val command = readCommand()

                context.findById(objectId).foreach { o =>
                  command match {
                    case GetType => getObjectType(o)
                    case SetField => setObjectField(o)
                    case GetField => getObjectField(o)
                    case GetState => getObjectState(o)
                    case SetState => setObjectState(o)
                    case HasField => objectHasField(o)
                    case HasState => objectHasState(o)
                    case _ =>
                  }
                }
              }
            }
          }.getOrElse(breaks.break())
        }
      }
    }
  }

  private final def getObjectType(o: EveStructuredObject) = {
    val t = readValue()
    writeValue(o.getType())
  }

  private final def getObjectField(o: EveStructuredObject) = {
    val field = readValue()
    writeResultValue(o.get(field))
  }

  private final def setObjectField(o: EveStructuredObject) = {
    val field = readValue()
    val value = readObject()
    o.set(field, value)
  }

  private final def getObjectState(o: EveStructuredObject) = {
    val state = readValue()
    writeResultValue(o.getState(state))
  }

  private final def setObjectState(o: EveStructuredObject) = {
    val state = readValue()
    val value = readValue()
    o.setState(state, value)
  }

  private final def objectHasField(o: EveStructuredObject) = {
    val field = readValue()
    writeValue(o.has(field))
  }

  private final def objectHasState(o: EveStructuredObject) = {
    val state = readValue()
    writeValue(o.hasState(state))
  }

  private def readCommand(): String = {
    val commandData = new Array[Byte](4)
    if(is.read(commandData) >= 0){
      new String(commandData)
    } else {
      null
    }
  }

  override def findItemsOfType(t: String): Option[EveObject] = context.findItemsOfType(t)
}

object StreamContext {
  def connect(host: String, port: Int)(context: QueryContext): Try[StreamContext] = Try {
    val socket = new Socket(host, port)
    new StreamContext(socket, context)
  }
}
