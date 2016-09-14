package com.ideal.evecore.universe.receiver

import com.ideal.evecore.common.Mapping.Mapping


/**
  * Created by Christophe on 14/07/2016.
  */
trait Receiver {
  def initReceiver(): Unit
  def handleMessage(message: Message)
  def destroyReceiver(): Unit
  def getMappings(): Mapping
}

object Receiver {

}
