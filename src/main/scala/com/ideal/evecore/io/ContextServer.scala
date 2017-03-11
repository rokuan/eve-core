package com.ideal.evecore.io

import java.net.ServerSocket
import java.util.UUID

import com.ideal.evecore.interpreter.Environment
import com.ideal.evecore.interpreter.remote.RemoteContext
import ContextServer._
import scala.util.control.Breaks

/**
 * Created by Christophe on 08/03/17.
 */
class ContextServer(protected val environment: Environment, port: Int) extends Thread {
  protected val server = new ServerSocket(port)
  private val contextGroup = new ThreadGroup(UUID.randomUUID().toString)

  override def run(): Unit = {
    super.run()
    val breaks = new Breaks

    breaks.breakable {
      while(true){
        val client = server.accept()
        val context = new RemoteContext(client)
        environment.addContext(context)
        val runnable = new Runnable {
          override def run(): Unit = {
            try {
              while (!context.closeIfDown()) {
                Thread.sleep(PingDelay)
              }
            } catch {
              case _: Throwable =>
            }
            environment.removeContext(context)
          }
        }
        new Thread(contextGroup, runnable).start()
      }
    }
  }

  def close() = contextGroup.destroy()
}

object ContextServer {
  val PingDelay = 300000
}