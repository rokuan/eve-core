package com.ideal.evecore.io

import java.net.ServerSocket

import com.ideal.evecore.interpreter.remote.RemoteReceiver
import com.ideal.evecore.universe.World

import scala.util.control.Breaks

/**
 * Created by chris on 09/03/17.
 */
class ReceiverServer(protected val world: World, port: Int) extends Thread {
  protected val server = new ServerSocket(port)

  override def run(): Unit = {
    super.run()
    val breaks = new Breaks

    breaks.breakable {
      while(true){
        val client = server.accept()
        val receiver = new RemoteReceiver(client)
      }
    }
  }
}
