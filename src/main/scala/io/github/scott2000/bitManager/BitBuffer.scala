package io.github.scott2000.bitManager

class BitBuffer {
  var buffer: Short = 0
  var offset: Int = 0
  var bits: Int = 0

  private final def localPositionOf(position: Int): Int = (position+offset)%16

  private final def getLocal(local: Int): Boolean = {
    ((buffer >>> local) & 1) == 1
  }

  private final def resizeLeft(resize: Int): Unit = {
    this.offset -= resize
    while (this.offset < 0) {
      this.offset += 16
    }
    while (this.offset >= 16) {
      this.offset -= 16
    }
    bits += resize
  }

  final def previous: Int = 15
  final def first: Int = 0
  final def last: Int = bits-1
  final def next: Int = bits
  final def size: Int = bits

  final def set(position: Int, value: Boolean = true): Unit = {
    val local = localPositionOf(position)
    if (position >= bits) {
      bits = (position+1).toByte
    }
    if (value) {
      buffer = (buffer | (1 << local)).toShort
    } else {
      buffer = (buffer & ~(1 << local)).toShort
    }
  }

  final def get(position: Int): Boolean = {
    getLocal(localPositionOf(position))
  }

  final def pullFirst(): Boolean = {
    val bit = getLocal(localPositionOf(first))
    resizeLeft(-1)
    bit
  }

  final def pullLast(): Boolean = {
    val bit = getLocal(localPositionOf(last))
    bits -= 1
    bit
  }

  final def pushLeft(value: Byte): Unit = {
    resizeLeft(8)
    for (position <- 0 until 8) {
      set(7-position, ((value >>> position) & 1) == 1)
    }
  }

  final def pushRight(value: Byte): Unit = {
    val bits = this.bits
    for (position <- 0 until 8) {
      set(bits+7-position, ((value >>> position) & 1) == 1)
    }
  }

  final def pullLeft(): Byte = {
    var out: Byte = 0
    for (position <- 0 until 8) {
      if (get(position)) {
        out = (out | (1 << (7-position))).toByte
      }
    }
    resizeLeft(-8)
    out
  }

  final def pullRight(): Byte = {
    var out: Byte = 0
    for (position <- 0 until 8) {
      if (get(bits-8+position)) {
        out = (out | (1 << (7-position))).toByte
      }
    }
    bits -= 8
    out
  }

  final def printRaw(append: String = s" Offset=$offset, Bits=$bits"): Unit = {
    for (position <- 0 until 16) {
      if ((position+(16-offset))%16 >= bits) {
        print('_')
      } else {
        if (getLocal(position)) {
          print('1')
        } else {
          print('0')
        }
      }
    }
    println(append)
  }
}