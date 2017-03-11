package com.ideal.evecore.interpreter.remote

import java.io.{InputStream, OutputStream}
import java.net.Socket

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.Readers.EveObjectResultConverter
import com.ideal.evecore.io.Streamers.{EveObjectStreamHandler, MessageStreamHandler}
import com.ideal.evecore.io.{StreamReader, StreamWriter}
import com.ideal.evecore.io.message.{Result, ResultReader, ResultWriter}
import org.json4s.jackson.JsonMethods
import org.json4s.native.Serialization._
import com.ideal.evecore.io.Readers._

/**
 * Created by Christophe on 06/03/17.
 */
trait StreamUtils {
  protected val socket: Socket

  val is: InputStream = socket.getInputStream
  val os: OutputStream = socket.getOutputStream

  implicit val resultConverter = new EveObjectResultConverter(socket)
  implicit val resultStreamHandler = new EveObjectStreamHandler(socket)
  implicit val messageConverter = new MessageConverter(socket)
  implicit val messageHandler = new MessageStreamHandler(socket)

  protected def writeValue(b: Boolean) = {
    os.write(if(b){ 1 } else { 0 })
    os.flush()
  }

  protected def writeValue(s: String) = {
    writeSize(s.length)
    os.write(s.getBytes)
    os.flush()
  }

  protected def writeSize(size: Int) = {
    val sizeData = new Array[Byte](4)
    (0 until sizeData.length).foreach(index => sizeData(index) = ((size >> (index * 8)) & 0xFF).toByte)
    os.write(sizeData)
    os.flush()
  }

  protected def writeObject(o: EveObject) = {
    val json = write(o)
    writeValue(json)
  }

  protected def readSize() = {
    val data = new Array[Byte](4)
    if(is.read(data) != 1) {
      data.zipWithIndex.foldLeft(0) { case (acc, (size, index)) => acc + ((size & 0xFF) << (index * 8)) }
    } else {
      0
    }
  }

  protected def readObject(): EveObject = {
    val json = readValue()
    resultConverter.extract(JsonMethods.parse(json))
  }

  protected def readValue(): String = {
    var length = readSize()
    val block = new Array[Byte](1024)
    val buffer = new StringBuilder()

    while(length > 0){
      val read = is.read(block)

      if(read > 0) {
        buffer.append(new String(block, 0, read))
        length -= read
      } else {
        length = 0
      }
    }

    buffer.toString()
  }

  protected def readTest(): Boolean = (is.read() != 0)

  /**
    * Reads a command from the server
    * @return
    */
  protected def readCommand(): String = {
    val commandData = new Array[Byte](4)
    if(is.read(commandData) >= 0){
      new String(commandData)
    } else {
      null
    }
  }

  protected def readResultValue[T >: Null](implicit reader: ResultReader[T]): Result[T] = reader.readFrom(is)
  protected def writeResultValue[T >: Null](v: Result[T])(implicit writer: ResultWriter[T]): Unit = writer.writeTo(os, v)

  protected def readItem[T](implicit reader: StreamReader[T]): T = reader.readFrom(is)
  protected def writeItem[T](o: T)(implicit writer: StreamWriter[T]): Unit = writer.writeTo(os, o)

  protected final def safe[T](process: T) = socket.synchronized(process)
}
