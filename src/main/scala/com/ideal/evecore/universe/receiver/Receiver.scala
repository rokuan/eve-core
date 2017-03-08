package com.ideal.evecore.universe.receiver

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.universe.ValueMatcher

import scala.util.Try


/**
  * Created by Christophe on 14/07/2016.
  */
trait Receiver {
  /**
   * Called to initialize this receiver
   */
  def initReceiver(): Unit

  /**
   * Called when destroying this receiver, to make sure everything is cleaned up
   */
  def destroyReceiver(): Unit

  /**
   * Executes the message
   * @param message The message to process
   * @return The result of the operation
   */
  def handleMessage(message: Message): Try[EveObject]

  /**
   * Retrieves this receiver's name
   * @return
   */
  def getReceiverName(): String = getClass.getName

  /**
   * Returns the mapping defining the types of messages this receiver can handle
   * @return A mapping containing the definition field of this receiver
   */
  def getMappings(): Mapping[_ <: ValueMatcher]

  override def toString: String = getReceiverName()
  override def equals(obj: scala.Any): Boolean = obj match {
    case null => false
    case r: Receiver => Option(getReceiverName()).map(_.equals(r.getReceiverName())).getOrElse(false)
    case _ => false
  }
}