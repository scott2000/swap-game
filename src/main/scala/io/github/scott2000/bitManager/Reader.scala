package io.github.scott2000.bitManager

/**
  * Created by Scott on 7/19/2017.
  */
trait Reader[T] {
  def read(args: Any*)(implicit reader: BitReader): T
}