package com.ideal.evecore.interpreter.remote

import java.io.{OutputStream, InputStream}
import java.net.Socket

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.message.Readers.EveObjectResultReader
import com.ideal.evecore.io.message.ResultReader
import org.json4s.native.Serialization._
import com.ideal.evecore.io.message.Readers._

/**
 * Created by chris on 06/03/17.
 */
trait StreamUtils {
  protected val socket: Socket

  val is: InputStream = socket.getInputStream
  val os: OutputStream = socket.getOutputStream

  implicit val resultReader = new EveObjectResultReader(socket)

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

  protected def readObject(): EveObject = { null /* TODO */ }

  protected def readValue(): String = {
    val source = scala.io.Source.fromInputStream(is)
    var length = readSize()
    val block = new Array[Byte](1024)
    val buffer = new StringBuilder()
    var read = 0

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

  protected def readResultValue[T >: Null](implicit reader: ResultReader[T]): Option[T] = {
    val result = reader.readFrom(is)
    if(result.success){
      None
    } else {
      Some(result.value)
    }
  }

  protected final def safe[T](process: T) = socket.synchronized(process)
}
