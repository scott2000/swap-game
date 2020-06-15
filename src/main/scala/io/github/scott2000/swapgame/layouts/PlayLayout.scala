package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.content._
import View.{GONE, VISIBLE}

object PlayLayout {
  val challengeColorRequirement = 5
  def isNeeded: Boolean = ColorManager.unlocked.length >= challengeColorRequirement
}

class PlayLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Play

  private val title           = MenuLayout.title("Play").wrap
  private val normalButton    = SButton(   "Normal", start(false)).wrap
  private val challengeButton = SButton("Challenge", start(true)).wrap
  private val backButton      = SButton(     "Back", back).wrap

  refresh()

  def start(challenge: Boolean): Unit = {
    Grid.load(challenge)
    MenuActivity.switchTo(Game)
  }

  override def refresh(): Unit = backButton.setBackgroundTintList(colorStateList)

  override def setHidden(): Unit = {
    alpha = 0.0f
    visibility = GONE
  }

  override def showFrom(previous: MenuLayout): Unit = {
    visibility = VISIBLE
    animate().alpha(1.0f).setListener(onEnd {})
  }

  override def hideForAnd(next: MenuLayout, action: => Unit): Unit = {
    animate().alpha(0.0f).setListener(onEnd {
      visibility = GONE
      action
    })
  }

  override def longBack(): Boolean = {
    Grid.barSwap = !Grid.barSwap
    true
  }

  override def back(): Boolean = {
    MenuActivity.switchTo(Menu)
    true
  }
}
