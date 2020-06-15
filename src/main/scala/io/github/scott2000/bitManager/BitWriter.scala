package io.github.scott2000.bitManager

import java.io.{File, FileOutputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

class BitWriter(outputStream: OutputStream) {
  var buffer: BitBuffer = new BitBuffer()

  implicit val writer: BitWriter = this

  def this(file: File) = this(new FileOutputStream(file))

  private final def writeBit(bit: Boolean): BitWriter = {
    buffer.set(buffer.next, bit)
    if (buffer.size >= 8) {
      flush()
    }
    this
  }

  private final def writeByte(byte: Byte): BitWriter = {
    buffer.pushRight(byte)
    if (buffer.size >= 8) {
      flush()
    }
    this
  }

  private final def flush(): Unit = {
    val pulled = buffer.pullLeft()
    var out = 0
    if (pulled < 0) {
      out = 256+pulled
    } else {
      out = pulled
    }
    outputStream.write(out)
  }

  private final def writeShort (value: Short):  BitWriter = write(ByteBuffer.wrap(new Array[Byte](2)).putShort(value))
  private final def writeInt   (value: Int):    BitWriter = write(ByteBuffer.wrap(new Array[Byte](4)).putInt(value))
  private final def writeLong  (value: Long):   BitWriter = write(ByteBuffer.wrap(new Array[Byte](8)).putLong(value))
  private final def writeFloat (value: Float):  BitWriter = write(ByteBuffer.wrap(new Array[Byte](4)).putFloat(value))
  private final def writeDouble(value: Double): BitWriter = write(ByteBuffer.wrap(new Array[Byte](8)).putDouble(value))

  final def write(bits: Array[Boolean]): BitWriter = {
    for (bit <- bits) {
      writeBit(bit)
    }
    this
  }

  final def write(bytes: Array[Byte]): BitWriter = {
    for (byte <- bytes) {
      writeByte(byte)
    }
    this
  }

  final def write(bytes: ByteBuffer): BitWriter = {
    write(bytes.array)
  }

  final def write(value: String, fixed: Boolean = false): BitWriter = {
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    if (!fixed) {
      write(bytes.length)
    }
    write(bytes)
  }

  final def write(value: AnyVal): BitWriter = {
    value match {
      case bit: Boolean   => writeBit(bit)
      case byte: Byte     => writeByte(byte)
      case int: Int       => writeInt(int)
      case short: Short   => writeShort(short)
      case char: Char     => writeShort(char.toShort)
      case long: Long     => writeLong(long)
      case float: Float   => writeFloat(float)
      case double: Double => writeDouble(double)
      case alt: Writable  => write(alt.asInstanceOf[Writable])
      case v              => throw new IllegalArgumentException(s"${v.getClass.getName} does not extend Writable.")
    }
    this
  }

  final def write(values: Array[AnyVal]): BitWriter = {
    for (value <- values) {
      write(value)
    }
    this
  }

  final def write(value: Writable): BitWriter = {
    value.writeWith(this)
    this
  }

  final def write[T](value: T, writer: Writer[T], args: Any*): BitWriter = {
    writer.write(value, args: _*)(this)
    this
  }

  final def close(): Unit = {
    if (isOpen) {
      write(true)
      while (buffer.size > 0) {
        write(false)
      }
      buffer = null
      outputStream.close()
    }
  }

  final def isOpen: Boolean = buffer != null
}
