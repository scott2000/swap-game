package io.github.scott2000.bitManager

/**
  * Created by Scott on 7/19/2017.
  */
trait Writer[T] {
  def write(value: T, args: Any*)(implicit writer: BitWriter): Unit
}
