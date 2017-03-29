package com.ideal.evecore.interpreter.remote

import java.io.IOException

import com.ideal.evecore.io.Serializers
import com.ideal.evecore.io.command.PingCommand

import scala.util.{Failure, Success, Try}

/**
  * Created by Christophe on 11/03/2017.
  */
trait RemoteEndPoint extends StreamUtils {
  implicit val formats = Serializers.buildBasicFormats()

  /**
    * Pings the client to ensure that the connection is still alive
    *
    * @return true if the connection is closed, false otherwise
    */
  def closeIfDown(): Boolean = Try {
    safe(writeUserCommand(PingCommand()))
  } match {
    case Success(_) => false
    case Failure(_: IOException) =>
      try {
        socket.close()
      } catch {
        case _: Throwable =>
      }
      true
    case _ => false
  }
}