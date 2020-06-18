package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.{SurfaceHolder, MotionEvent, View}
import android.graphics.Canvas
import android.content.Context

object GameView {
  var threadID = -1
}

class GameView(implicit ctx: Context) extends SSurfaceView with SurfaceHolder.Callback with Runnable {
  import GameView._

  var thread: Option[Thread] = None
  var lastTouch: Option[PositionFraction] = None
  var lastUpdate: Long = System.currentTimeMillis

  holder.addCallback(this)

  onTouch((view, event) => {
    for (grid <- Grid.grid) {
      if (event.getHistorySize > 0) {
        for (n <- event.getHistorySize - 1 to 0 by -1) {
          grid.touch(PositionFraction(event.getHistoricalX(n), event.getHistoricalY(n)))
        }
      }
      lastTouch = if (event.getAction == MotionEvent.ACTION_UP) {
        grid.untouch()
        None
      } else {
        val position = PositionFraction(event.getX, event.getY)
        grid.touch(position)
        Some(position)
      }
    }
    draw()
    true
  })

  override def onDraw(canvas: Canvas): Unit = drawWith(canvas)

  def drawWith(canvas: Canvas): Unit = {
    canvas.drawColor(TileType.backgroundColor)
    for (grid <- Grid.grid) {
      grid.displayOn(canvas, lastTouch)
    }
  }

  def draw(): Unit = {
    val nextUpdate = System.currentTimeMillis
    val time = nextUpdate-lastUpdate
    for (grid <- Grid.grid if time > 0) {
      grid.update(time)
    }
    if (holder.getSurface.isValid) {
      val canvas = holder.lockCanvas()
      if (canvas != null) try {
        drawWith(canvas)
      } finally holder.unlockCanvasAndPost(canvas)
    }
    lastUpdate = nextUpdate
  }

  def start(): Unit = {
    setZOrderOnTop(true)
    visibility = View.VISIBLE
    if (thread.isEmpty) {
      val thread = new Thread(this)
      this.thread = Some(thread)
      thread.start()
    }
  }

  def pause(): Unit = {
    for (thread <- this.thread) {
      this.thread = None
      thread.join()
    }
    Settings.save()
    for (grid <- Grid.grid) {
      grid.save()
    }
  }

  def stop(): Unit = {
    setZOrderOnTop(false)
    visibility = View.GONE
    pause()
  }

  override def run(): Unit = {
    threadID += 1
    val id = threadID
    println(s"Starting GameView Thread $id...")
    while (thread.contains(Thread.currentThread())) {
      if (threadID != id) {
        MenuActivity.instance.runOnUiThread { stop() }
      }
      draw()
      Thread.sleep(7)
    }
    println(s"Stopping GameView Thread $id...")
  }

  override def surfaceCreated(holder: SurfaceHolder): Unit = {}
  override def surfaceDestroyed(holder: SurfaceHolder): Unit = {}
  override def surfaceChanged(holder: SurfaceHolder, f: Int, w: Int, h: Int): Unit = {}
}
