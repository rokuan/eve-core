package com.ideal.evecore.util

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/**
 * Created by Christophe on 06/04/2017.
 */

trait PendingAtomicValue[T] {
  protected val modified = new AtomicBoolean(false)

  def set(v: T): Unit
  def get(): T
}

class PendingAtomicBoolean extends PendingAtomicValue[Boolean] {
  protected val value = new AtomicBoolean()

  override def set(v: Boolean): Unit = {
    value.set(v)
    modified.set(true)
    value.synchronized(value.notify())
  }

  override def get(): Boolean = {
    while(!modified.get()){
      value.synchronized(value.wait())
    }
    value.get()
  }
}

class PendingAtomicReference[T] extends PendingAtomicValue[T] {
  protected val value = new AtomicReference[T]

  override def set(v: T): Unit = {
    value.set(v)
    modified.set(true)
    value.synchronized(value.notify())
  }

  override def get(): T = {
    while(!modified.get()){
      value.synchronized(value.wait())
    }
    value.get()
  }
}
