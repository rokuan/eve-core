package com.ideal.evecore.io.message

import java.io.{OutputStream, InputStream}
import com.ideal.evecore.io.Readers
import org.json4s.jackson.JsonMethods
import org.json4s._
import JsonDSL._

/**
 * Created by Christophe on 05/03/2017.
 */
case class Result[T >: Null](success: Boolean, value: T)

trait ResultReader[T >: Null] {
  import Readers._

  private def readSize(is: InputStream): Int = {
    val sizeArray = new Array[Byte](4)
    if(is.read(sizeArray) != sizeArray.length){
      0
    } else {
      sizeArray.zipWithIndex
        .foldLeft(0){ case (acc, (size, index)) => acc + ((size & 0xFF) << (index * 8)) }
    }
  }

  final def readFrom(is: InputStream): Result[T] = {
    var size = readSize(is)
    val buffer = new StringBuffer()
    val data = new Array[Byte](1024)

    while(size > 0){
      val read = is.read(data)
      if(read > 0) {
        buffer.append(new String(data, 0, read))
      } else {
        size = 0
      }
    }

    val json = JsonMethods.parse(buffer.toString)
    val success = (json \ "success").extractOpt[Boolean].getOrElse(false)

    if(success){
      Result[T](true, extract(json \ "value"))
    } else {
      Result[T](false, null)
    }
  }

  def extract(o: JValue): T
}

trait ResultWriter[T >: Null] {
  private def writeSize(os: OutputStream, size: Int): Unit = {
    val sizeData = new Array[Byte](4)
    (0 until sizeData.length).foreach(index => sizeData(index) = ((size >> (index * 8)) & 0xFF).toByte)
    os.write(sizeData)
    os.flush()
  }

  final def writeTo(os: OutputStream, r: Result[T]): Unit = {
    val valueJson = if(r.success){ Some(transform(r.value)) } else { Option.empty[JValue] }
    val json = ("success" -> r.success) ~ ("value" -> valueJson)
    val str = JsonMethods.compact(json)
    writeSize(os, str.length)
    os.write(str.getBytes)
    os.flush()
  }

  def transform(o: T): JValue
}
