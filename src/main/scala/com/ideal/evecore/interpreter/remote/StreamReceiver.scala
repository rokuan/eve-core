package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{Message, Receiver}
import com.ideal.evecore.io.Streamers._
import com.ideal.evecore.common.Conversions._

import scala.util.Try
import scala.util.control.Breaks

/**
  * Created by Christophe on 11/03/2017.
  */
class StreamReceiver(protected val socket: Socket, protected val receiver: Receiver) extends Receiver with StreamUtils {
  readEndlessly()

  private final def readEndlessly() = Try {
    val breaks = new Breaks

    breaks.breakable {
      while(true){
        safe {
          Option(readCommand()).map { command =>
            command match {
              case RemoteReceiverMessage.GetMappings => {
                val mappings = getMappings()
                writeItem(mappings)
              }
              case RemoteReceiverMessage.HandleMessage => {
                val message = readItem[Message]
                val result = handleMessage(message)
                writeResultValue(result)
              }
              case RemoteEndPointMessage.Ping =>
              case RemoteReceiverMessage.GetReceiverName => {
                val name = getReceiverName()
                writeValue(name)
              }
              case RemoteReceiverMessage.InitReceiver => initReceiver()
              case RemoteReceiverMessage.DestroyReceiver => destroyReceiver()
              case _ =>
            }
          }
        }
      }
    }
  }

  /**
    * Called to initialize this receiver
    */
  override def initReceiver(): Unit = receiver.initReceiver()

  /**
    * Retrieves this receiver's name
    *
    * @return This receiver's name
    */
  override def getReceiverName(): String = receiver.getReceiverName()

  /**
    * Returns the mapping defining the types of messages this receiver can handle
    *
    * @return A mapping containing the definition field of this receiver
    */
  override def getMappings(): Mapping[ValueMatcher] = receiver.getMappings()

  /**
    * Executes the message
    *
    * @param message The message to process
    * @return The result of the operation
    */
  override def handleMessage(message: Message): Try[EveObject] = receiver.handleMessage(message)

  /**
    * Called when destroying this receiver, to make sure everything is cleaned up
    */
  override def destroyReceiver(): Unit = receiver.destroyReceiver()
}

object StreamReceiver {
  def connect(host: String, port: Int)(receiver: Receiver): Try[StreamReceiver] = Try {
    val socket = new Socket(host, port)
    new StreamReceiver(socket, receiver)
  }
}