package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.{View, KeyEvent}
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
    println(s"Switching State: ${MenuActivity.state} -> $state")
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
    if (!isConnected && !isConnecting) {
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
    Settings.shouldConnect = true
    Settings.update()
    layout.changeAPI()
  }

  override def onConnectionFailed(connectionResult: ConnectionResult): Unit = {
    isConnected = false
    if (connectionResult.hasResolution() && !isConnecting && Settings.shouldConnect) {
      try {
        connectionResult.startResolutionForResult(this, RC_ERROR_RESOLVE)
      } catch {
        case exception: IntentSender.SendIntentException => apiClient.connect()
      }
    } else {
      isConnecting = false
    }
    layout.changeAPI()
  }

  override def onActivityResult(i: Int, state: Int, intent: Intent): Unit = {
    if (i == RC_ERROR_RESOLVE) {
      if (state == Activity.RESULT_OK) {
        apiClient.connect()
      } else if (state == Activity.RESULT_CANCELED) {
        isConnecting = false
        Settings.shouldConnect = false
      }
      layout.changeAPI()
    }
  }

  override def onConnectionSuspended(i: Int): Unit = {
    isConnecting = true
    isConnected = false
    layout.changeAPI()
    apiClient.connect()
  }

  def layout: MenuLayout = states(state)

  lazy val states = Map(
    Menu     -> new MainMenuLayout(),
    Play     -> new PlayLayout(),
    Game     -> new GameLayout(),
    Color    -> new ColorLayout(),
    GameOver -> new GameOverLayout()
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
  }

  onStop {
    endConnection()
    layout.clean()
  }

  onPause {
    Settings.save()
    for (grid <- Grid.grid) {
      grid.save()
    }
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

  private def toggleFullscreen(): Unit = {
    if (isFullscreen) {
      exitFullscreen()
    } else {
      enterFullscreen()
    }
  }

  private def continueFullscreen(): Unit = {
    if (isFullscreen) {
      enterFullscreen()
    } else {
      exitFullscreen()
    }
  }

  override def onKeyLongPress(keyCode: Int, event: KeyEvent): Boolean = {
    if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_BACK) {
      layout.longBack()
    } else {
      false
    }
  }

  override def onBackPressed(): Unit = {
    if (!layout.back()) {
      super.onBackPressed()
    }
  }
}
