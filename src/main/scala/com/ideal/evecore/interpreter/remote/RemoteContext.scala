package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{Context, EveObject}

import scala.util.Try

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteContext(val socket: Socket) extends Context with StreamUtils {
  import RemoteContextMessage._

  override def findItemsOfType(t: String): Option[EveObject] = {
    writeCommand(FindItemsOfType)
    writeValue(t)
    readResultValue[EveObject]
  }

  protected def writeCommand(cmd: String) = {
    os.write(cmd.getBytes)
    os.flush()
  }
}

object RemoteContextMessage {
  val FindItemsOfType = "FTYP"
  val ObjectRequest = "ORQT"
}
