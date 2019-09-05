package io.github.scott2000.swapgame

import io.github.scott2000.bitManager.{Reader, Writable, BitWriter, BitReader}

object Position {
  def apply(x: Int, y: Int): PositionWhole = PositionWhole(x, y)
  def apply(x: Float, y: Float): PositionFraction = PositionFraction(x, y)
}

object PositionWhole extends Reader[PositionWhole] {
  override def read(args: Any*)(implicit reader: BitReader): PositionWhole = PositionWhole(reader.readInt(), reader.readInt())
}

case class PositionWhole(x: Int, y: Int) extends Writable {
  def distanceTo(position: PositionWhole): Double = {
    val xDiff = position.x.toDouble-x.toDouble
    val yDiff = position.y.toDouble-y.toDouble
    math.sqrt(math.pow(xDiff, 2) + math.pow(yDiff, 2))
  }

  def to(position: PositionWhole, mod: Float): PositionFraction = {
    def modify(a: Int, b: Int): Float = a*(1-mod)+b*mod
    PositionFraction(modify(x, position.x), modify(y, position.y))
  }

  def ==(that: PositionWhole): Boolean = this.x == that.x && this.y == that.y
  def !=(that: PositionWhole): Boolean = this.x != that.x || this.y != that.y

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(x)
    writer.write(y)
  }

  override def toString: String = s"($x, $y)"
}

object PositionFraction extends Reader[PositionFraction] {
  override def read(args: Any*)(implicit reader: BitReader): PositionFraction = PositionFraction(reader.readFloat(), reader.readFloat())
}

case class PositionFraction(x: Float, y: Float) extends Writable {
  def distanceTo(position: PositionFraction): Double = {
    val xDiff = position.x.toDouble-x.toDouble
    val yDiff = position.y.toDouble-y.toDouble
    math.sqrt(math.pow(xDiff, 2) + math.pow(yDiff, 2))
  }

  def to(position: PositionFraction, mod: Float): PositionFraction = {
    def modify(a: Float, b: Float): Float = a*(1-mod)+b*mod
    PositionFraction(modify(x, position.x), modify(y, position.y))
  }

  def ==(that: PositionFraction): Boolean = this.x == that.x && this.y == that.y
  def !=(that: PositionFraction): Boolean = this.x != that.x || this.y != that.y

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(x)
    writer.write(y)
  }

  override def toString: String = f"($x%.1f, $y%.1f)"
}
