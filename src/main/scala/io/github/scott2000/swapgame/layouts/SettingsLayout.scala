package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.content._
import View.{GONE, VISIBLE}
import android.animation.LayoutTransition
import android.net.Uri

class SettingsLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Options

  private val title          = MenuLayout.title("Settings").wrap
  private val darkModeButton = SButton(              ?, toggleDark).wrap
  private val tutorialButton = SButton("Show Tutorial", showTutorial).wrap
  private val scoresButton   = SButton(  "Leaderboard", leaderboard).wrap
  private val websiteButton  = SButton( "Open Website", openWebsite).wrap
  private val backButton     = SButton(         "Back", back).wrap

  refresh()

  layoutTransition = new LayoutTransition()

  def toggleDark(): Unit = {
    Settings.darkMode = !Settings.darkMode
    Settings.save()
    MenuActivity.instance.mergedLayout.backgroundColor = TileType.backgroundColor
    refresh()
  }

  def showTutorial(): Unit = {
    Settings.enableTutorial()
    Grid.create(false)
    MenuActivity.switchTo(Game)
  }

  private def leaderboard(): Unit = {
    MenuActivity.instance.showLeaderboard()
  }

  def openWebsite(): Unit = {
    val website = Uri.parse("https://github.com/scott2000/swap-game")
    val intent = new Intent(Intent.ACTION_VIEW, website)
    if (intent.resolveActivity(ctx.getPackageManager) != null) ctx.startActivity(intent)
  }

  override def refresh(): Unit = {
    changeAPI()
    MenuLayout.updateTitle(title)
    backButton.setBackgroundTintList(colorStateList)
    if (Settings.darkMode) {
      darkModeButton.text = "Light Mode"
    } else {
      darkModeButton.text = "Dark Mode"
    }
    if (Settings.tutorial) {
      tutorialButton.visibility = GONE
    } else {
      tutorialButton.visibility = VISIBLE
    }
  }

  override def changeAPI(): Unit = {
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
    animate().alpha(1.0f).setListener(onEnd {})
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
