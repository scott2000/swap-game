package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.content._
import View.{GONE, VISIBLE}

class GameLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Game

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
  override def pause(): Unit = gameView.pause()
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

  override def back(): Boolean = {
    MenuActivity.switchTo(if (PlayLayout.isNeeded) Play else Menu)
    true
  }
}
