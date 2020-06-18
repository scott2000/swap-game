package io.github.scott2000.swapgame

import io.github.scott2000.bitManager._

import scala.collection.mutable.ArrayBuffer

object ColorManager extends BitObject {
  private var _uiColor: UIColor = UIColor.Red
  private var _unlocked: ArrayBuffer[UIColor] = ArrayBuffer(UIColor.Red)

  override def read(args: Any*)(implicit reader: BitReader): Unit = {
    _uiColor = reader.read(UIColor)
    for (_ <- 0 until reader.readByte()) {
      unlock(reader.read(UIColor))
    }
  }

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(_uiColor)
    writer.write(_unlocked.size.toByte)
    for (color <- _unlocked) {
      writer.write(color)
    }
  }

  def uiColor: UIColor = _uiColor
  def uiColor_=(uiColor: UIColor): Unit = _uiColor = uiColor

  def color: Int = uiColor.color

  def unlocked: Array[UIColor] = _unlocked.toArray
  def unlock(uiColor: UIColor): Unit = {
    if (!_unlocked.contains(uiColor)) {
      _unlocked += uiColor
    }
  }

  def unlockAll(score: Int, chain: Int = 0, combo: Int = 0): Array[UIColor] = {
    val colorBuffer = new ArrayBuffer[UIColor]()
    for (color <- UIColor.colors if color.requirement.shouldUnlock(score, chain, combo) && !_unlocked.contains(color)) {
      _unlocked += color
      Settings.unlockAchievement(color)
      colorBuffer += color
      if (_unlocked.length == PlayLayout.challengeColorRequirement) {
        // represents "challenge unlocked"
        colorBuffer += UIColor.Red
      }
    }
    colorBuffer.toArray
  }
}
