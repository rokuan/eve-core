package com.ideal.evecore.common


object Mapping {
  type Mapping[T] = Map[String, T]
  implicit def setToMapping[T](s: Set[(String, T)]): Mapping[T] = s.toMap
}