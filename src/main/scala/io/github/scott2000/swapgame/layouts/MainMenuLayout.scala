package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.animation.LayoutTransition
import android.content._
import View.{GONE, VISIBLE}

class MainMenuLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Menu

  private val title          = MenuLayout.title("Swap").onClick(titleClick).wrap
  private val playButton     = SButton(    "Play", play).wrap
  private val colorsButton   = SButton(  "Colors", colors).wrap
  private val scoresButton   = SButton(  "Scores", MenuActivity.instance.showLeaderboard).wrap
  private val settingsButton = SButton("Settings", settings).wrap

  // If a user clicks the title exactly 12 times, show difficulty level
  private val clickCountTarget = 12
  private var clickCount = 0

  refresh()

  layoutTransition = new LayoutTransition()

  private def play(): Unit = {
    if (PlayLayout.isNeeded) {
      MenuActivity.switchTo(Play)
    } else {
      MenuActivity.layout[PlayLayout](Play).start(false)
    }
  }

  private def colors(): Unit = MenuActivity.switchTo(Color)

  private def settings(): Unit = MenuActivity.switchTo(Options)

  private def titleClick(): Unit = {
    clickCount += 1
    Settings.showDifficultyLevel = clickCount == clickCountTarget
    clickCount %= clickCountTarget
  }

  override def refresh(): Unit = {
    changeAPI()
    MenuLayout.updateTitle(title)
    playButton.setBackgroundTintList(colorStateList)
  }

  override def changeAPI(): Unit = {
    colorsButton.visibility = if (ColorManager.unlocked.length > 1) VISIBLE else GONE
    if (MenuActivity.isConnected) {
      scoresButton.visibility = VISIBLE
    } else {
      scoresButton.visibility = GONE
    }
  }

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
}
