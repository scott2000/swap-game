package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.content._
import View.{GONE, VISIBLE}

class ColorLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Color

  private val colorView = new ColorView(this)
  this += colorView

  override def refresh(): Unit = colorView.start()
  override def pause(): Unit = colorView.stop()
  override def clean(): Unit = colorView.stop()

  override def setHidden(): Unit = {
    alpha = 0.0f
    visibility = GONE
  }

  override def showFrom(previous: MenuLayout): Unit = {
    visibility = VISIBLE
    animate().alpha(1.0f).setListener(doNothing)
  }

  override def hideForAnd(next: MenuLayout, action: => Unit): Unit = {
    animate().alpha(0.0f).setListener(onEnd {
      visibility = GONE
      action
    })
  }

  override def back(): Boolean = {
    MenuActivity.switchTo(Menu)
    true
  }
}
