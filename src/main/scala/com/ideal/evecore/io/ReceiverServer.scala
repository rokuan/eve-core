package com.ideal.evecore.io

import java.net.ServerSocket
import java.util.UUID

import com.ideal.evecore.interpreter.remote.RemoteReceiver
import com.ideal.evecore.universe.World

import ReceiverServer._
import scala.util.control.Breaks

/**
 * Created by Christophe on 09/03/17.
 */
class ReceiverServer(protected val world: World, port: Int) extends Thread {
  protected val server = new ServerSocket(port)
  private val receiverGroup = new ThreadGroup(UUID.randomUUID().toString)

  override def run(): Unit = {
    super.run()
    val breaks = new Breaks

    breaks.breakable {
      while(true){
        val client = server.accept()
        val receiver = new RemoteReceiver(client)
        world.registerReceiver(receiver)
        val runnable = new Runnable {
          override def run(): Unit = {
            try {
              while (!receiver.closeIfDown()) {
                Thread.sleep(PingDelay)
              }
            } catch {
              case _: Throwable =>
            }
            world.unregisterReceiver(receiver)
          }
        }
        new Thread(receiverGroup, runnable).start()
      }
    }
  }

  def close() = receiverGroup.destroy()
}

object ReceiverServer {
  val PingDelay = 300000
}