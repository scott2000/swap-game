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
  val version: Byte = 5
  val settingsFile = "settings"
  val leaderboards = Array("CgkItbTPhNUKEAIQBw", "CgkItbTPhNUKEAIQCA", "CgkItbTPhNUKEAIQCQ")
  private val minLeaderboard = Array(1, 1, 3)
  private val _highScores = Array(0, 0, 0)
  private var _tutorial = true
  private var _shouldConnect = true
  var darkMode = false

  var submitLeaderboard = Array(true, true, true)
  var submitAchievement = true

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
    if (newScore > _highScores(index)) {
      _highScores(index) = newScore
    }
    submitScore(newScore, index)
  }

  def scoreFor(isChallenge: Boolean): Int = _highScores(indexForLeaderboard(isChallenge))

  def submitScore(score: Int, index: Int): Unit = {
    if (score >= minLeaderboard(index)) {
      if (MenuActivity.isConnected) {
        Games.Leaderboards.submitScore(MenuActivity.instance.apiClient, leaderboards(index), score)
      } else if (score > _highScores(index)) {
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
      for (id <- color.requirement.id) {
        Games.Achievements.unlock(MenuActivity.instance.apiClient, id)
      }
    } else {
      submitAchievement = true
    }
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
      for (index <- submitLeaderboard.indices if submitLeaderboard(index)) {
        restoreLeaderboardScore(index)
        submitScore(_highScores(index), index)
      }
      if (submitAchievement) {
        restoreAchievements()
        for (color <- ColorManager.unlocked) {
          unlockAchievement(color)
        }
        for (color <- UIColor.colors.reverseIterator if !ColorManager.unlocked.contains(color)) {
          for (id <- color.requirement.id) {
            Games.Achievements.reveal(MenuActivity.instance.apiClient, id)
          }
        }
      }
      if (MenuActivity.isConnected) {
        for (index <- submitLeaderboard.indices) {
          submitLeaderboard(index) = false
        }
        submitAchievement = false
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
    _highScores(0) = reader.readInt()
    _highScores(1) = reader.readInt()
    if (version >= 3) {
      _highScores(comboLeaderboard) = reader.readInt()
    }
    _tutorial = reader.read()
    if (version >= 4) {
      for (index <- submitLeaderboard.indices) {
        submitLeaderboard(index) = reader.read()
      }
      if (version >= 5) {
        submitAchievement = reader.read()
      } else {
        // Before this update, achievements were all hidden by default
        reader.read()
      }
    } else {
      // It doesn't matter if it was supposed to be up to date or not
      reader.read()
    }
    if (version >= 1) {
      _shouldConnect = reader.read()
      if (version >= 2) {
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
