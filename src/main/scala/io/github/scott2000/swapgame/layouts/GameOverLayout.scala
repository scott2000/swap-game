package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.{View, Gravity}
import android.text.{Spanned, SpannableString}
import android.text.style.ForegroundColorSpan
import android.graphics.Typeface
import android.content._
import View.{GONE, VISIBLE}

import scala.collection.mutable.ArrayBuffer

object GameOverLayout {
  private var messageText: String = ?
  private var scoreText: String = ?
  private var chainText: String = ?

  private var unlockColorText: Option[Array[CharSequence]] = Some(Array(new SpannableString(?)))

  private var highScoreText: Option[String] = Some(?)
}

class GameOverLayout()(implicit ctx: Context) extends MenuLayout {
  import GameOverLayout._

  override def uuid: State = GameOver

  private def formatSub(text: STextView): STextView = {
    MenuLayout.transparentBackground(text)
      .textSize(Grid.subSize)
      .typeface(Grid.typeface)
  }

  private val messageBox = MenuLayout.title(?).wrap
  private val scoreBox = formatSub(STextView(?)).wrap
  private val highScoreBox = formatSub(STextView(?)).wrap
  private val chainBox = formatSub(STextView(?)).wrap
  private val unlockColor = formatSub(STextView(?)).gravity(Gravity.CENTER).singleLine(false).wrap
  private val mainMenu = SButton("Main Menu", back).wrap
  mainMenu.marginTop(24)
  unlockColor.setTypeface(null, Typeface.BOLD)

  refresh()

  private def setColorUnlockDisplay(colors: Array[UIColor]): Unit = {
    if (colors.isEmpty) {
      unlockColorText = None
    } else {
      val text = new ArrayBuffer[CharSequence]()
      var first = true
      for (color <- colors) {
        if (first) {
          first = false
        } else {
          text += "\n"
        }
        val str = {
          if (color == UIColor.Red)
            new SpannableString("Unlocked Challenge Mode!")
          else
            new SpannableString(s"Unlocked ${color.name} (${color.requirement})")
        }

        str.setSpan(new ForegroundColorSpan(color.color), 0, str.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        text += str
      }
      unlockColorText = Some(text.toArray)
    }
  }

  def endTutorial(): Unit = {
    this.synchronized {
      messageText = "Tutorial Over"
      scoreText = "You are now ready to play."
      highScoreText = None
      chainText = "Good luck!"
      setColorUnlockDisplay(Array())
    }
  }

  def updateGameStats(highScore: Int, score: Int, bestChain: Int, bestCombo: Int, colors: Array[UIColor], leveler: Leveler): Unit = {
    this.synchronized {
      val isRecord = score > highScore
      messageText = {
        if (isRecord && highScore != 0)
          "New Record"
        else
          "Game Over"
      }
      scoreText = s"Score: $score"
      highScoreText = {
        if (isRecord)
          None
        else
          Some(s"Record: $highScore")
      }
      chainText = {
        if (bestCombo > 20)
          s"Highest Combo: $bestCombo"
        else
          s"Longest Chain: $bestChain"
      }
      setColorUnlockDisplay(colors)
    }
  }

  override def refresh(): Unit = {
    this.synchronized {
      MenuLayout.updateTitle(messageBox)
      MenuLayout.updateTitle(scoreBox)
      MenuLayout.updateTitle(highScoreBox)
      MenuLayout.updateTitle(chainBox)
      mainMenu.setBackgroundTintList(colorStateList)
      messageBox.text = messageText
      scoreBox.text = scoreText
      highScoreText match {
        case None =>
          highScoreBox.visibility = GONE
        case Some(text) =>
          highScoreBox.text = text
          highScoreBox.visibility = VISIBLE
      }
      chainBox.text = chainText
      if (unlockColorText.isDefined) {
        unlockColor.text = unlockColorText.get(0)
        for (n <- 1 until unlockColorText.get.length) {
          unlockColor.append(unlockColorText.get(n))
        }
        unlockColor.visibility = VISIBLE
      } else {
        unlockColor.visibility = GONE
      }
    }
  }

  override def setHidden(): Unit = {
    alpha = 0.0f
    visibility = GONE
  }

  override def showFrom(previous: MenuLayout): Unit = {
    Grid.grid = None
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
