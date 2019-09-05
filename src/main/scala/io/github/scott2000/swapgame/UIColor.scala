package io.github.scott2000.swapgame

import io.github.scott2000.bitManager._

object UIColor extends Reader[UIColor] {
  val colors = Array(Red, Orange, Green, Cyan, Blue, Violet, Magenta)

  override def read(args: Any*)(implicit reader: BitReader): UIColor = {
    val index = reader.readByte()
    for (color <- colors) {
      if (color.index == index) {
        return color
      }
    }
    if (colors.indices.contains(index.toInt)) colors(index) else colors(0)
  }

  final object Red     extends UIColor(0xd82020, "Red",      0, TutorialRequirement)
  final object Orange  extends UIColor(0xff8800, "Orange",  10, ChainRequirement(   8, Some("CgkItbTPhNUKEAIQAQ")))
  final object Green   extends UIColor(0x55bb20, "Green",   20, ScoreRequirement( 500, Some("CgkItbTPhNUKEAIQAg")))
  final object Cyan    extends UIColor(0x20c8b0, "Cyan",    30, ChainRequirement(  12, Some("CgkItbTPhNUKEAIQAw")))
  final object Blue    extends UIColor(0x0098ff, "Blue",    40, ScoreRequirement(1000, Some("CgkItbTPhNUKEAIQBA")))
  final object Violet  extends UIColor(0x8000b8, "Violet",  50, ChainRequirement(  16, Some("CgkItbTPhNUKEAIQBQ")))
  final object Magenta extends UIColor(0xc800a0, "Magenta", 60, ScoreRequirement(2000, Some("CgkItbTPhNUKEAIQBg")))
}

sealed class UIColor(protected var _color: Int, val name: String, val index: Byte, val requirement: UnlockRequirement) extends Writable {
  override def writeWith(writer: BitWriter): Unit = {
    writer.write(index)
  }

  def color = _color | 0xff000000
  def toHex: String = f"${_color & 0xffffff}%06x"
  override def toString: String = name
}
