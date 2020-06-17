package io.github.scott2000.swapgame

import java.io.File

import io.github.scott2000.bitManager._

import com.google.android.gms.games._
import com.google.android.gms.games.leaderboard.Leaderboards

import android.content.Context

object Settings extends BitObject {
  private var isLoaded = false
  val version: Byte = 3
  val leaderboards = Array("CgkItbTPhNUKEAIQBw", "CgkItbTPhNUKEAIQCA")
  val comboLeaderboard = "CgkItbTPhNUKEAIQCQ"
  val settingsFile = "settings"
  private val _highScores = Array(0, 0)
  private var _bestCombo = 0
  private var _tutorial = true
  var upToDate = true
  var shouldConnect = true
  var darkMode = false

  def addScore(newScore: Int, isChallenge: Boolean): Unit = {
    val index = if (isChallenge) 1 else 0
    if (newScore > _highScores(index)) {
      _highScores(index) = newScore
      submitScore(isChallenge)
    }
  }

  def scoreFor(isChallenge: Boolean): Int = {
    val index = if (isChallenge) 1 else 0
    _highScores(index)
  }

  def submitScore(isChallenge: Boolean): Unit = {
    val index = if (isChallenge) 1 else 0
    val score = _highScores(index)
    if (score >= 1) {
      if (MenuActivity.isConnected) {
        Games.Leaderboards.submitScore(MenuActivity.instance.apiClient, leaderboards(index), score)
      } else {
        upToDate = false
      }
    }
  }

  def addCombo(newCombo: Int): Unit = {
    if (newCombo > _bestCombo) {
      _bestCombo = newCombo
      submitCombo()
    }
  }

  def submitCombo(): Unit = {
    if (_bestCombo >= 6) {
      if (MenuActivity.isConnected) {
        Games.Leaderboards.submitScore(MenuActivity.instance.apiClient, comboLeaderboard, _bestCombo)
      } else {
        upToDate = false
      }
    }
  }

  def unlockAchievement(color: UIColor): Unit = {
    if (MenuActivity.isConnected) {
      for (id <- color.requirement.id) {
        Games.Achievements.unlock(MenuActivity.instance.apiClient, id)
      }
    } else {
      upToDate = false
    }
  }

  def update(): Unit = {
    if (!upToDate && MenuActivity.isConnected) {
      submitScore(false)
      submitScore(true)
      submitCombo()
      for (color <- ColorManager.unlocked) {
        unlockAchievement(color)
      }
      if (MenuActivity.isConnected) {
        upToDate = true
      }
    }
  }

  def tutorial: Boolean = _tutorial
  def disableTutorial(): Unit = _tutorial = false
  def enableTutorial(): Unit = _tutorial = true

  def hasSave(implicit ctx: Context): Boolean = new File(ctx.getFilesDir, settingsFile).exists()

  def load()(implicit ctx: Context): Unit = if (!isLoaded) {
    if (hasSave) {
      val bitReader = new BitReader(ctx.openFileInput(settingsFile))
      bitReader.read(Settings)
      bitReader.close()
    }
    isLoaded = true
  }

  def save()(implicit ctx: Context): Unit = {
    val bitWriter = new BitWriter(ctx.openFileOutput(settingsFile, Context.MODE_PRIVATE))
    bitWriter.write(Settings)
    bitWriter.close()
  }

  override def read(args: Any*)(implicit reader: BitReader): Unit = {
    val version = reader.readByte()
    _highScores(0) = math.max(reader.readInt(), _highScores(0))
    _highScores(1) = math.max(reader.readInt(), _highScores(1))
    if (version >= 3) {
      _bestCombo = reader.readInt()
    }
    _tutorial = reader.read()
    upToDate = reader.read()
    if (version >= 1) {
      shouldConnect = reader.read()
      if (version >= 2) {
        darkMode = reader.read()
      }
    }
    reader.read(ColorManager, version)
  }

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(version)
    writer.write(scoreFor(false))
    writer.write(scoreFor(true))
    writer.write(_bestCombo)
    writer.write(_tutorial)
    writer.write(upToDate)
    writer.write(shouldConnect)
    writer.write(darkMode)
    writer.write(ColorManager)
  }
}
