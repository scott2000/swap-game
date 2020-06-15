package io.github.scott2000

import scala.util.Random

import android.view.View
import android.animation.{Animator, AnimatorListenerAdapter}

import android.content.res.ColorStateList
import android.R.attr._

package object swapgame {
  val random = new Random()
  def animateTime = if (_debug) 3000.0f else 250.0f
  var _debug = false

  def ? = "Error"

  type State = Option[Int]
  val Menu:     State = None
  val Play:     State = Some(0)
  val Game:     State = Some(1)
  val Color:    State = Some(2)
  val GameOver: State = Some(3)

  def onEnd(action: => Unit) = new AnimatorListenerAdapter() {override def onAnimationEnd(animation: Animator): Unit = action}

  lazy val disableStateList: ColorStateList = {
    new ColorStateList(Array(
      Array(-state_enabled),
      new Array[Int](0)
    ), Array(
      setAlpha(TileType.strokeColor, 0.25f),
      TileType.strokeColor
    ))
  }

  def colorStateList: ColorStateList = {
    new ColorStateList(Array(
      Array(-state_enabled),
      new Array[Int](0)
    ), Array(
      setAlpha(TileType.strokeColor, 0.25f),
      ColorManager.color
    ))
  }

  def towards(from: Float, to: Float, speed: Float = 1.0f, time: Long): Float = towards(from, to, speed*time/animateTime)

  def towards(from: Float, to: Float, distance: Float): Float = {
    if (from < to) {
      math.min(to, from+distance)
    } else if (from > to) {
      math.max(to, from-distance)
    } else to
  }

  def setAlpha(color: Int, alpha: Float): Int = (((color >>> 24)*alpha).toInt << 24) | (color & 0x00ffffff)

  def colorAverage(from: Int, to: Int, part: Float = 0.5f): Int = {
    def shiftAverage(shift: Int): Int = (math.round(((from >>> shift) & 0xff)*(1-part)+((to >>> shift) & 0xff)*part) & 0xff) << shift
    shiftAverage(24) | shiftAverage(16) | shiftAverage(8) | shiftAverage(0)
  }
}
