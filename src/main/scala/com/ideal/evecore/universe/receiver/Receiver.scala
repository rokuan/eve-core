package com.ideal.evecore.universe.receiver

/**
  * Created by Christophe on 14/07/2016.
  */
trait Receiver {
  def initReceiver(): Unit
  def handleMessage(message: EveMessage)
  def destroyReceiver(): Unit
  def getMappings(): Seq[Mapping]
}

object Receiver {

}
