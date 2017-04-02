package com.ideal.evecore.interpreter.remote

import java.net.Socket

/**
  * Created by Christophe on 03/04/2017.
  */
class SocketWrapper(val socket: Socket) extends BasicSocketUtils {
  override def writeValue(b: Boolean): Unit = super.writeValue(b)
  override def writeValue(s: String): Unit = super.writeValue(s)
  override def readValue(): String = super.readValue()
  override def readTest(): Boolean = super.readTest()
}
