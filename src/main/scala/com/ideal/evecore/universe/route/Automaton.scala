package com.ideal.evecore.universe.route

/**
  * Created by Christophe on 15/07/2016.
  */
trait Automaton[BranchType, T] {
  def add(o: T)
  def remove(o: T)
  def find(o: BranchType): Option[T]
}
