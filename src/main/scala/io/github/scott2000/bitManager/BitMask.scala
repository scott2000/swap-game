package io.github.scott2000.bitManager

import scala.collection.mutable.ArrayBuffer
import scala.math.log

/**
  * Created by Scott Taylor on 6/25/2017.
  */

object BitMask {
  def apply(values: BigInt): BitMask                = BitMask(0, values)
  def apply(range: Range): BitMask                  = BitMask(range.start, range.length)
  def apply(start: BigInt, length: BigInt): BitMask = new BitMask((log((length-1).toDouble)/log(2)).toInt+1, start)

  case class MaskOverflowException(value: BigInt, mask: BitMask) extends Exception(s"Value outside of mask range: $value with $mask")
}

class BitMask(val bits: Int, val offset: BigInt = 0) extends Saver[BigInt] {
  import BitMask.MaskOverflowException

  val bytes: Int = bits/8 + (if (bits%8 != 0) 1 else 0)

  final def apply(in: BigInt): BigInt    = in-offset
  final def unapply(out: BigInt): BigInt = out+offset

  final override def read(args: Any*)(implicit reader: BitReader): BigInt = {
    val bytes = new ArrayBuffer[Byte]
    val difference = bits%8
    if (difference != 0) {
      var byte: Byte = 0
      for (bit <- 7 to 0 by -1) {
        if (bit < difference && reader.read()) {
          byte = (byte | (1 << bit)).toByte
        }
      }
      bytes += byte
    }
    val others = bits/8
    for (byte <- reader.readBytes(others)) {
      bytes += byte
    }
    unapply(BigInt(Array(0x00: Byte) ++ bytes.toArray[Byte]))
  }

  final override def write(value: BigInt, args: Any*)(implicit writer: BitWriter): Unit = {
    val bytes = this(value).toByteArray
    val total = bytes.length*8
    if (bits < total) {
      var cut = bits % 8
      for (byte <- bytes.length - this.bytes until bytes.length) {
        if (cut != 0) {
          for (bit <- 7 to 0 by -1) {
            val b = ((bytes(byte) >>> bit) & 1) == 1
            if (bit < cut) {
              writer.write(b)
            } else if (b) {
              throw MaskOverflowException(value, this)
            }
          }
          cut = 0
        } else {
          writer.write(bytes(byte))
        }
      }
    } else {
      if (bits > total) {
        for (_ <- 0 until bits - total) {
          writer.write(false)
        }
      }
      writer.write(bytes)
    }
  }

  final override def toString: String = f"WriteMask($bits%2x, $offset%x)"
}
