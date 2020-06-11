package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.{View, Gravity, KeyEvent}
import android.widget.LinearLayout
import android.animation.{Animator, AnimatorListenerAdapter, LayoutTransition}
import android.content._
import android.content.Intent
import android.net.Uri
import View.{GONE, VISIBLE}

class MainMenuLayout()(implicit ctx: Context) extends MenuLayout {
  override def uuid: State = Menu

  gravity = Gravity.CENTER
  orientation = LinearLayout.VERTICAL

  def openWebsite(): Unit = {
    val website = Uri.parse("https://github.com/scott2000/swap-game")
    val intent = new Intent(Intent.ACTION_VIEW, website)
    if (intent.resolveActivity(ctx.getPackageManager) != null) ctx.startActivity(intent)
  }

  private val title        = MenuLayout.clickableTitle("Swap", openWebsite).wrap
  private val playButton   = SButton(   "Play", play).wrap
  private val colorsButton = SButton( "Colors", colors).wrap
  private val scoresButton = SButton( "Scores", leaderboard).wrap
  private val signInButton = SButton("Sign In", leaderboard).wrap

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
  private def leaderboard(): Unit = {
    if (MenuActivity.isConnected) {
      MenuActivity.instance.showLeaderboard()
    } else {
      MenuActivity.instance.startSignIn()
    }
  }

  override def refresh(): Unit = {
    changeAPI()
    colorsButton.visibility = if (ColorManager.unlocked.length > 1) VISIBLE else GONE
    playButton.setBackgroundTintList(colorStateList)
  }

  override def changeAPI(): Unit = {
    if (MenuActivity.isConnecting) {
      scoresButton.visibility = GONE
      signInButton.visibility = GONE
    } else if (MenuActivity.isConnected) {
      scoresButton.visibility = VISIBLE
      signInButton.visibility = GONE
    } else {
      scoresButton.visibility = GONE
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
