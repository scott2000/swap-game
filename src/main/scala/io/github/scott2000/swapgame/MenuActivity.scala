package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.View
import android.os.Bundle
import android.app.Activity
import android.content._

import com.google.android.gms._
import com.google.android.gms.common.ConnectionResult
import com.google.android.gms.common.api.GoogleApiClient
import com.google.android.gms.games._

import com.google.android.gms.auth.api.signin._
import com.google.android.gms.auth.api._
import com.google.android.gms.auth._

object MenuActivity {
  val outFullscreen: Int = 0
  val inFullscreen: Int = outFullscreen | View.SYSTEM_UI_FLAG_FULLSCREEN | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY

  def switchTo(state: State): Boolean = {
    for (instance <- _instance) {
      if (instance.layout to MenuActivity(state)) {
        MenuActivity.state = state
        return true
      }
    }
    false
  }

  def layout[T](state: State): T = MenuActivity(state).asInstanceOf[T]

  def apply(state: State): MenuLayout = instance.states(state)

  def instance: MenuActivity = _instance.get
  private def instance_=(instance: MenuActivity) = _instance = Some(instance)

  def state: State = _state
  private def state_=(state: State): Unit = _state = state

  def isConnected: Boolean = _isConnected
  private def isConnected_=(isConnected: Boolean): Unit = _isConnected = isConnected

  def isConnecting: Boolean = _isConnecting
  private def isConnecting_=(isConnecting: Boolean): Unit = _isConnecting = isConnecting

  private var _instance: Option[MenuActivity] = None
  private var _state: State = Menu
  private var _isConnected = false
  private var _isConnecting = false
}

class MenuActivity extends SActivity with GoogleApiClient.ConnectionCallbacks with GoogleApiClient.OnConnectionFailedListener {
  import MenuActivity._

  val RC_SIGN_IN = 5001
  val RC_ERROR_RESOLVE = 5002
  val RC_LEADERBOARD = 5003

  lazy val apiClient = new GoogleApiClient.Builder(this)
    .addConnectionCallbacks(this)
    .addOnConnectionFailedListener(this)
    .addApi(Games.API).addScope(Games.SCOPE_GAMES)
    .build()

  def startConnection(): Unit = {
    if (Settings.shouldConnect && !isConnected && !isConnecting) {
      isConnecting = true
      apiClient.connect()
    }
    layout.changeAPI()
  }

  def startSignIn(): Unit = {
    if (!isConnected) {
      Settings.shouldConnect = true
      startConnection()
    }
  }

  def endConnection(): Unit = {
    if (isConnected) {
      isConnecting = false
      isConnected = false
      apiClient.disconnect()
    }
    layout.changeAPI()
  }

  override def onConnected(connectionHint: Bundle): Unit = {
    isConnecting = false
    isConnected = true
    Settings.update()
    Settings.save()
    layout.changeAPI()
  }

  override def onConnectionSuspended(i: Int): Unit = {
    isConnecting = false
    isConnected = false
    layout.changeAPI()
  }

  override def onConnectionFailed(connectionResult: ConnectionResult): Unit = {
    isConnected = false
    if (Settings.shouldConnect) {
      try {
        connectionResult.startResolutionForResult(this, RC_ERROR_RESOLVE)
      } catch {
        case exception: IntentSender.SendIntentException => {
          System.out.println("Games API resolution => SendIntentException")
          Settings.shouldConnect = false
          Settings.save()
          isConnecting = false
        }
      }
    } else {
      isConnecting = false
    }
    layout.changeAPI()
  }

  override def onActivityResult(requestCode: Int, responseCode: Int, data: Intent): Unit = {
    if (requestCode == RC_ERROR_RESOLVE) {
      if (responseCode == Activity.RESULT_OK) {
        apiClient.connect()
      } else {
        if (responseCode == Activity.RESULT_CANCELED) {
          Settings.shouldConnect = false
          Settings.save()
        } else {
          System.out.println("Games API resolution failed")
        }
        isConnecting = false
      }
      layout.changeAPI()
    } else if (responseCode == GamesActivityResultCodes.RESULT_RECONNECT_REQUIRED) {
      isConnected = false
      apiClient.disconnect()
      Settings.shouldConnect = false
      Settings.save()
      layout.changeAPI()
    }
  }

  def layout: MenuLayout = states(state)

  lazy val states = Map(
    Menu     -> new MainMenuLayout(),
    Play     -> new PlayLayout(),
    Game     -> new GameLayout(),
    Color    -> new ColorLayout(),
    GameOver -> new GameOverLayout(),
    Options  -> new SettingsLayout()
  )

  lazy val mergedLayout = new SFrameLayout {
    for ((_, layout) <- states) {
      layout.here.fill
    }
  }

  var isFullscreen = false

  override def onCreate(savedInstanceState: Bundle): Unit = {
    setTheme(R.style.AppTheme)
    super.onCreate(savedInstanceState)
    Settings.load()
    mergedLayout.backgroundColor = TileType.backgroundColor
    instance = this
    contentView = mergedLayout
    enterFullscreen()
    for ((_, layout) <- states) {
      layout.set(state)
    }
  }

  onStart {
    startConnection()
    layout.refresh()
  }

  onResume {
    continueFullscreen()
    layout.refresh()
  }

  onStop {
    endConnection()
    layout.clean()
  }

  onPause {
    layout.pause()
  }

  def showLeaderboard(): Unit = if (isConnected) {
    startActivityForResult(Games.Leaderboards.getAllLeaderboardsIntent(apiClient), RC_LEADERBOARD)
  }

  private def enterFullscreen(): Unit = {
    isFullscreen = true
    getWindow.getDecorView.setSystemUiVisibility(inFullscreen)
  }

  private def exitFullscreen(): Unit = {
    isFullscreen = false
    getWindow.getDecorView.setSystemUiVisibility(outFullscreen)
  }

  private def continueFullscreen(): Unit = {
    if (isFullscreen) {
      enterFullscreen()
    } else {
      exitFullscreen()
    }
  }

  override def onBackPressed(): Unit = {
    if (!layout.back()) {
      super.onBackPressed()
    }
  }
}
