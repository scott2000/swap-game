package io.github.scott2000.bitManager

/**
  * Created by Scott Taylor on 6/24/2017.
  */
object Unsigned {
  def apply(byte: Byte): BigInt   = BigInt(byte)  & 0xffL
  def apply(short: Short): BigInt = BigInt(short) & 0xffffL
  def apply(int: Int): BigInt     = BigInt(int)   & 0xffffffffL
  def apply(long: Long): BigInt   = BigInt(long)  & 0xffffffffffffffffL

  final def multiplyFraction(multiplicand: Int, multiplier: Short): Long = (Unsigned(multiplicand)*Unsigned(multiplier)).toLong
  final def padLeftTo(string: String, length: Int, char: Char = '0'): String = string.reverse.padTo(length, char).reverse
}
