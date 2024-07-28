package io.github.scott2000.swapgame

import java.io.File

import io.github.scott2000.bitManager._

import com.google.android.gms.games._
import com.google.android.gms.common.api.ResultCallback
import com.google.android.gms.games.achievement.{Achievement, Achievements}
import com.google.android.gms.games.leaderboard.{LeaderboardVariant, Leaderboards}

import android.content.Context

object Settings extends BitObject {
  private var isLoaded = false
  val version: Byte = 6
  val settingsFile = "settings"
  val leaderboards = Array("CgkItbTPhNUKEAIQBw", "CgkItbTPhNUKEAIQCA", "CgkItbTPhNUKEAIQCQ")
  private val minLeaderboard = Array(1, 1, 3)
  private val _highScores = Array(0, 0, 0)
  private var _tutorial = true
  private var _shouldConnect = true
  var darkMode = false

  // Not saved
  var showDifficultyLevel = false

  private var submitLeaderboard = Array(true, true, true)
  private var submitAchievement = true

  val comboLeaderboard = 2

  def shouldConnect: Boolean = _shouldConnect
  def shouldConnect_=(connect: Boolean): Unit = {
    if (connect) {
      _shouldConnect = true
    } else {
      submitLeaderboard = Array(true, true, true)
      submitAchievement = true
      _shouldConnect = false
    }
  }

  def indexForLeaderboard(isChallenge: Boolean): Int = {
    if (isChallenge) 1 else 0
  }

  def addScore(newScore: Int, index: Int): Unit = {
    submitScore(newScore, index)
    if (newScore > _highScores(index)) {
      _highScores(index) = newScore
    }
  }

  def scoreFor(isChallenge: Boolean): Int = _highScores(indexForLeaderboard(isChallenge))

  def submitScore(score: Int, index: Int): Unit = {
    if (score >= minLeaderboard(index)) {
      if (MenuActivity.isConnected) {
        try {
          Games.Leaderboards.submitScore(MenuActivity.instance.apiClient, leaderboards(index), score)
          return
        } catch {
          case e: Exception =>
            e.printStackTrace()
        }
      }
      if (score > _highScores(index)) {
        submitLeaderboard(index) = true
      }
    }
  }

  def addCombo(newCombo: Int): Unit = {
    addScore(newCombo, comboLeaderboard)
  }

  def addComboEarly(combo: Int): Unit = {
    if (combo > 16 && combo > _highScores(comboLeaderboard)) {
      addCombo(combo)
    }
  }

  def unlockAchievement(color: UIColor): Unit = {
    if (MenuActivity.isConnected) {
      try {
        for (id <- color.requirement.id) {
          Games.Achievements.unlock(MenuActivity.instance.apiClient, id)
        }
        return
      } catch {
        case e: Exception =>
          e.printStackTrace()
      }
    }
    submitAchievement = true
  }

  def restoreLeaderboardScore(index: Int): Unit = {
    Games.Leaderboards.loadCurrentPlayerLeaderboardScore(
      MenuActivity.instance.apiClient,
      leaderboards(index),
      LeaderboardVariant.TIME_SPAN_ALL_TIME,
      LeaderboardVariant.COLLECTION_PUBLIC)
      .setResultCallback(new ResultCallback[Leaderboards.LoadPlayerScoreResult]() {
        override def onResult(r: Leaderboards.LoadPlayerScoreResult): Unit = {
          implicit val ctx: Context = MenuActivity.instance.ctx
          if (r.getStatus().isSuccess()) {
            val score = r.getScore()
            if (score != null) {
              val rawScore = score.getRawScore().toInt
              if (rawScore > _highScores(index)) {
                if (index == 0 && rawScore > 100) {
                  disableTutorial()
                }
                _highScores(index) = rawScore
                ColorManager.unlockAll(rawScore)
                MenuActivity.instance.runOnUiThread { MenuActivity.instance.layout.changeAPI() }
              }
            }
          } else {
            submitLeaderboard(index) = true
          }
          Settings.save()
        }
      })
  }

  def restoreAchievements(): Unit = {
    Games.Achievements.load(MenuActivity.instance.apiClient, false)
      .setResultCallback(new ResultCallback[Achievements.LoadAchievementsResult]() {
        override def onResult(r: Achievements.LoadAchievementsResult): Unit = {
          implicit val ctx: Context = MenuActivity.instance.ctx
          if (r.getStatus().isSuccess()) {
            val buffer = r.getAchievements()
            val count = buffer.getCount()
            for (i <- 0 until count) {
              val achievement = buffer.get(i)
              if (achievement.getState() == Achievement.STATE_UNLOCKED) {
                val id = achievement.getAchievementId()
                for (color <- UIColor.colors) {
                  if (color.requirement.id.contains(id)) {
                    ColorManager.unlock(color)
                  }
                }
              }
            }
            buffer.close()
            MenuActivity.instance.runOnUiThread { MenuActivity.instance.layout.changeAPI() }
          } else {
            submitAchievement = true
          }
          r.release()
          Settings.save()
        }
      })
  }

  def update(): Unit = {
    _shouldConnect = true
    if (MenuActivity.isConnected) {
      try {
        for (index <- submitLeaderboard.indices if submitLeaderboard(index)) {
          submitScore(_highScores(index), index)
          restoreLeaderboardScore(index)
        }
        if (submitAchievement) {
          for (color <- ColorManager.unlocked) {
            unlockAchievement(color)
          }
          for (color <- UIColor.colors.reverseIterator if !ColorManager.unlocked.contains(color)) {
            for (id <- color.requirement.id) {
              Games.Achievements.reveal(MenuActivity.instance.apiClient, id)
            }
          }
          restoreAchievements()
        }
        if (MenuActivity.isConnected) {
          for (index <- submitLeaderboard.indices) {
            submitLeaderboard(index) = false
          }
          submitAchievement = false
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
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
    synchronized {
      val bitWriter = new BitWriter(ctx.openFileOutput(settingsFile, Context.MODE_PRIVATE))
      bitWriter.write(Settings)
      bitWriter.close()
    }
  }

  override def read(args: Any*)(implicit reader: BitReader): Unit = {
    val version = reader.readByte()
    _highScores(indexForLeaderboard(false)) = reader.readInt()
    _highScores(indexForLeaderboard(true)) = reader.readInt()
    if (version >= 3) {
      // Version 3 introduced a leaderboard for best combo
      _highScores(comboLeaderboard) = reader.readInt()
    }
    _tutorial = reader.read()
    if (version >= 6) {
      // Version 6 ignores the previous flags since they might not be correct
      for (index <- submitLeaderboard.indices) {
        submitLeaderboard(index) = reader.read()
      }
      submitAchievement = reader.read()
    } else {
      if (version >= 4) {
        // Version 4 replaced a single up-to-date flag with a set of more specific flags
        for (_ <- submitLeaderboard.indices) {
          reader.read()
        }
      }
      reader.read()
    }
    if (version >= 1) {
      // Version 1 added a flag to stop automatic sign in when cancelled
      _shouldConnect = reader.read()
      if (version >= 2) {
        // Version 2 introduced dark mode
        darkMode = reader.read()
      }
    }
    reader.read(ColorManager, version)
  }

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(version)
    for (score <- _highScores) {
      writer.write(score)
    }
    writer.write(_tutorial)
    for (submit <- submitLeaderboard) {
      writer.write(submit)
    }
    writer.write(submitAchievement)
    writer.write(_shouldConnect)
    writer.write(darkMode)
    writer.write(ColorManager)
  }
}
