package com.ideal.evecore.universe.route

import com.ideal.evecore.universe.ValueMatcher

trait State[T] {
  def getNext(): Set[(ValueMatcher, State[T])]
}