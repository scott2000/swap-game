package io.github.scott2000.bitManager

import java.io.{File, FileInputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

class BitReader(inputStream: InputStream) {
  var buffer: BitBuffer = new BitBuffer()

  def this(file: File) = this(new FileInputStream(file))

  final def read(): Boolean = {
    if (buffer.size == 0) {
      flush()
    }
    val bit = buffer.pullFirst()
    if (inputStream.available() == 0 && buffer.size == 0) {
      close()
    }
    bit
  }

  final def read(bits: Int): Array[Boolean] = {
    val bitArray = new Array[Boolean](bits)
    for (position <- 0 until bits) {
      bitArray(position) = read()
    }
    bitArray
  }

  final def read[T](reader: Reader[T], args: Any*): T = reader.read(args: _*)(this)

  final def readByte(): Byte = {
    if (buffer.size < 8) {
      flush()
    }
    val byte = buffer.pullLeft()
    if (inputStream.available() == 0 && buffer.size == 0) {
      close()
    }
    byte
  }

  final def readBytes(bytes: Int): Array[Byte] = {
    val byteArray = new Array[Byte](bytes)
    for (position <- 0 until bytes) {
      byteArray(position) = readByte()
    }
    byteArray
  }

  final def readShort():  Short  = readByteBuffer(ByteBuffer.wrap(new Array[Byte](2))).getShort(0)
  final def readInt():    Int    = readByteBuffer(ByteBuffer.wrap(new Array[Byte](4))).getInt(0)
  final def readLong():   Long   = readByteBuffer(ByteBuffer.wrap(new Array[Byte](8))).getLong(0)
  final def readFloat():  Float  = readByteBuffer(ByteBuffer.wrap(new Array[Byte](4))).getFloat(0)
  final def readDouble(): Double = readByteBuffer(ByteBuffer.wrap(new Array[Byte](8))).getDouble(0)
  final def readString():                 String = new String(readBytes(readInt()), StandardCharsets.UTF_8)
  final def readFixedString(length: Int): String = new String(readBytes(length),    StandardCharsets.UTF_8)

  final def readByteBuffer(buffer: ByteBuffer): ByteBuffer = {
    buffer.put(readBytes(buffer.remaining()))
  }

  final def flush(): Unit = {
    val in = inputStream.read()
    buffer.pushRight(in.toByte)
    if (inputStream.available() == 0) {
      var last = false
      do {
        last = buffer.pullLast()
      } while (!last)
    }
  }

  final def close(): Unit = {
    if (isOpen) {
      buffer = null
      inputStream.close()
    }
  }

  final def isOpen: Boolean = buffer != null
  final def remaining: Int = if (isOpen) inputStream.available() + buffer.size else 0
}
