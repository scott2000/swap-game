package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.{View, Gravity, KeyEvent}
import android.widget.LinearLayout
import android.animation.{Animator, AnimatorListenerAdapter, LayoutTransition}
import android.content._
import View.{GONE, VISIBLE}

class GameLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Game

  gravity = Gravity.CENTER
  orientation = LinearLayout.VERTICAL

  private var hasShown = false
  private var ready = true

  private val gameView = new GameView()
  this += gameView

  override def setVisible(): Unit = {
    hasShown = true
  }

  override def setHidden(): Unit = {
    visibility = GONE
  }

  override def refresh(): Unit = gameView.start()
  override def clean(): Unit = gameView.stop()

  override def showFrom(previous: MenuLayout): Unit = {
    ready = false
    visibility = VISIBLE
    if (!hasShown) {
      x = (x+previous.getWidth)
      hasShown = true
    }
    animate().xBy(-previous.getWidth).setListener(onEnd {
      ready = true
    })
  }

  override def hideForAnd(next: MenuLayout, action: => Unit): Unit = {
    ready = false
    animate().xBy(getWidth).setListener(onEnd {
      visibility = GONE
      ready = true
      action
    })
  }

  override def isReady: Boolean = ready

  override def longBack(): Boolean = {
    _debug = !_debug
    true
  }

  override def back(): Boolean = {
    MenuActivity.switchTo(if (PlayLayout.isNeeded) Play else Menu)
    true
  }
}
