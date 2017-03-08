package com.ideal.evecore.io

import java.net.ServerSocket

import com.ideal.evecore.interpreter.Environment
import com.ideal.evecore.interpreter.remote.RemoteContext

import scala.util.control.Breaks

/**
 * Created by Christophe on 08/03/17.
 */
class ContextServer(protected val environment: Environment, port: Int) extends Thread {
  protected val server = new ServerSocket(port)

  override def run(): Unit = {
    super.run()

    val breaks = new Breaks

    breaks.breakable {
      while(true){
        val client = server.accept()
        val context = new RemoteContext(client)
        environment.addContext(context)
        new ContextThread(environment, context).start
      }
    }
  }
}

class ContextThread(private val environment: Environment, private val context: RemoteContext) extends Thread {
  override def run(): Unit = {
    super.run()
    while(!context.closeIfDown()){
      Thread.sleep(300000)
    }
    environment.removeContext(context)
  }
}
