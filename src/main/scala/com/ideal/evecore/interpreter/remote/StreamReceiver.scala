package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Receiver}
import com.ideal.evecore.io.Streamers._
import com.ideal.evecore.common.Conversions._

import scala.util.Try

/**
 * Created by Christophe on 11/03/2017.
 */
class StreamReceiver(private val receiverId: String, protected val socket: Socket, protected val receiver: Receiver) extends Receiver with StreamUtils {
  final def handleCommand(command: ReceiverCommand) = Try {
    safe {
      command match {
        case c: GetMappingsCommand => {
          val mappings = getMappings()
          writeItem(mappings)
        }
        case c: HandleMessageCommand => {
          val result = handleMessage(c.message)
          writeResultValue(result)
        }
        case c: GetReceiverNameCommand => {
          val name = getReceiverName()
          writeValue(name)
        }
        case c: InitReceiverCommand => initReceiver()
        case c: DestroyReceiverCommand => destroyReceiver()
        case _ =>
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
  override def handleMessage(message: EveObjectMessage): Try[EveObject] = receiver.handleMessage(message)

  /**
   * Called when destroying this receiver, to make sure everything is cleaned up
   */
  override def destroyReceiver(): Unit = receiver.destroyReceiver()
}