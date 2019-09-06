package io.github.scott2000.bitManager

/**
  * Created by Scott Taylor on 6/25/2017.
  */
trait Writable {
  def writeWith(writer: BitWriter): Unit
}
