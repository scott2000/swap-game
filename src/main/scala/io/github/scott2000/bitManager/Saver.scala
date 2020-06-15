package io.github.scott2000.bitManager

/**
  * Created by Scott on 7/19/2017.
  */
trait Saver[T] extends Reader[T] with Writer[T]
