package com.ideal.evecore.interpreter.remote

import java.net.Socket

/**
  * Created by Christophe on 26/03/2017.
  */
trait BasicSocketUtils {
  protected val socket: Socket
  protected val is = socket.getInputStream
  protected val os = socket.getOutputStream

  protected def writeValue(b: Boolean) = {
    os.write(if(b){ 1 } else { 0 })
    os.flush()
  }

  protected def writeValue(s: String) = {
    writeSize(s.length)
    os.write(s.getBytes)
    os.flush()
  }

  private final def writeSize(size: Int) = {
    val sizeData = new Array[Byte](4)
    (0 until sizeData.length).foreach(index => sizeData(index) = ((size >> (index * 8)) & 0xFF).toByte)
    os.write(sizeData)
    os.flush()
  }

  private final def readSize() = {
    val data = new Array[Byte](4)
    if(is.read(data) != 1) {
      data.zipWithIndex.foldLeft(0) { case (acc, (size, index)) => acc + ((size & 0xFF) << (index * 8)) }
    } else {
      0
    }
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
}
