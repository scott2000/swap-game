package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.animation.LayoutTransition
import android.content._
import android.content.Intent
import android.net.Uri
import View.{GONE, VISIBLE}
import android.widget.Toast

class MainMenuLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Menu

  private val title          = MenuLayout.title("Swap").wrap
  private val playButton     = SButton(    "Play", play).wrap
  private val colorsButton   = SButton(  "Colors", colors).wrap
  private val settingsButton = SButton("Settings", settings).wrap
  private val signInButton   = SButton( "Sign In", signIn).wrap

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

  private def signIn(): Unit = {
    if (!MenuActivity.isConnected) {
      MenuActivity.instance.startSignIn()
    }
  }

  override def refresh(): Unit = {
    changeAPI()
    MenuLayout.updateTitle(title)
    colorsButton.visibility = if (ColorManager.unlocked.length > 1) VISIBLE else GONE
    playButton.setBackgroundTintList(colorStateList)
  }

  override def changeAPI(): Unit = {
    if (MenuActivity.isConnecting || MenuActivity.isConnected) {
      signInButton.visibility = GONE
    } else {
      signInButton.visibility = VISIBLE
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
}
