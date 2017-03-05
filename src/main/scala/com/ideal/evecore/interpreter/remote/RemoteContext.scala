package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{Context, EveObject}

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteContext(val socket: Socket) extends Context {
  val os = socket.getOutputStream
  val is = socket.getInputStream

  override def findItemsOfType(t: String): Option[EveObject] = {
    None // TODO
  }
}
