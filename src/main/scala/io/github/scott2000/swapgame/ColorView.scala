package io.github.scott2000.swapgame

import org.scaloid.common._
import android.view.{MotionEvent, View}
import android.graphics.{Canvas, Paint}
import android.content.Context

object ColorView {
  private var threadID = 0
  val offset = 360f/12
}

class ColorView(layout: ColorLayout)(implicit ctx: Context) extends SView with Runnable {
  import ColorView._

  val backButton = ctx.getResources().getDrawable(R.drawable.back_arrow)

  private var segments: Option[Array[UIColor]] = None
  private var unlocked = ColorManager.unlocked
  private var selector = 0.0f
  private var goalSelector = 0.0f
  private var thread: Option[Thread] = None
  private var lastUpdate = System.currentTimeMillis
  private var touchLocation: Option[PositionFraction] = None
  private var r = 0
  private var b = 0
  private var g = 0
  private var nextColor: Int = 0xffd82020

  def setSegments(): Unit = {
    this.synchronized {
      unlocked = ColorManager.unlocked
      segments = Some(UIColor.colors.sortWith((a: UIColor, b: UIColor) => {
        val ac = unlocked.contains(a)
        val bc = unlocked.contains(b)
        if (ac == bc) a.index < b.index
        else ac
      }))
      for (segments <- this.segments) {
        selector = ((360f-offset)/segments.length)*segments.indexOf(ColorManager.uiColor)+offset/2-90f
        goalSelector = selector
      }
      nextColor = ColorManager.color
      r = (nextColor >>> 16) & 0xff
      g = (nextColor >>>  8) & 0xff
      b =  nextColor         & 0xff
      MenuActivity.instance.runOnUiThread { invalidate() }
    }
  }

  onTouch((view, event) => {
    touchLocation = if (MenuActivity.state == Color) Some(PositionFraction(event.getX, event.getY)) else None
    true
  })

  override def onDraw(canvas: Canvas): Unit = {
    this.synchronized {
      val paint = new Paint()
      paint.setStyle(Paint.Style.FILL)
      paint.setAntiAlias(false)
      val outline = new Paint()
      outline.setStyle(Paint.Style.STROKE)
      outline.setAntiAlias(true)
      outline.setColor(TileType.strokeColor)
      outline.setStrokeWidth(3.0f)
      for (segments <- this.segments) {
        val radius = math.min(canvas.getWidth, canvas.getHeight)/2-Grid.indent
        val radius2 = radius-Grid.indent*2
        val size = (360f-offset)/segments.length
        for (touch <- touchLocation) {
          val x = touch.x-canvas.getWidth/2
          val y = touch.y-canvas.getHeight/2
          val r = math.sqrt(math.pow(x, 2)+math.pow(y, 2))
          if (r <= radius+Grid.indent/2 && r > Grid.cropTo) {
            var theta = (math.atan(y/x)*180/math.Pi).toFloat
            theta += 90f-offset/2
            if (x < 0) {
              theta += 180
            }
            if (theta > 360) {
              theta -= 360
            }
            val segment = (theta/size).toInt
            if (segment < segments.length && theta > 0) {
              if (ColorManager.uiColor != segments(segment) && unlocked.contains(segments(segment))) {
                ColorManager.uiColor = segments(segment)
                nextColor = ColorManager.color
                goalSelector = segment*size+offset/2-90f
              }
            } else if (r >= radius2-Grid.indent/2) {
              layout.back()
            }
            touchLocation = None
          }
        }
        var position = offset/2-90f
        for (uiColor <- segments) {
          paint.setColor(if (unlocked.contains(uiColor)) uiColor.color else 0)
          canvas.drawArc(canvas.getWidth/2-radius, canvas.getHeight/2-radius, canvas.getWidth/2+radius, canvas.getHeight/2+radius, position, size, true, paint)
          canvas.drawArc(canvas.getWidth/2-radius, canvas.getHeight/2-radius, canvas.getWidth/2+radius, canvas.getHeight/2+radius, position, size, true, outline)
          position += size
        }
        paint.setColor(0xffffffff)

        canvas.drawArc(canvas.getWidth/2-radius, canvas.getHeight/2-radius, canvas.getWidth/2+radius, canvas.getHeight/2+radius, position, offset, true, outline)
        canvas.drawArc(canvas.getWidth/2-radius2, canvas.getHeight/2-radius2, canvas.getWidth/2+radius2, canvas.getHeight/2+radius2, selector+size, 360f-size, true, paint)
        canvas.drawArc(canvas.getWidth/2-radius2, canvas.getHeight/2-radius2, canvas.getWidth/2+radius2, canvas.getHeight/2+radius2, selector+size, 360f-size, true, outline)
        canvas.drawCircle(canvas.getWidth/2, canvas.getHeight/2, Grid.cropTo, paint)
        canvas.drawCircle(canvas.getWidth/2, canvas.getHeight/2, Grid.cropTo, outline)

        paint.setColor((0xff << 24) | (r << 16) | (g << 8) | b)
        paint.setAntiAlias(true)
        canvas.drawCircle(canvas.getWidth/2, canvas.getHeight/2, Grid.tileSize/2, paint)
        backButton.setBounds(canvas.getWidth/2-36, canvas.getHeight/2-radius+Grid.indent-36, canvas.getWidth/2+36, canvas.getHeight/2-radius+Grid.indent+36)
        backButton.draw(canvas)
      }
    }
  }

  def update(time: Long): Unit = {
    this.synchronized {
      for (segments <- this.segments) {
        if (selector != goalSelector || ((r << 16) | (g << 8) | b) != (nextColor & 0xffffff) || touchLocation.isDefined) {
          MenuActivity.instance.runOnUiThread { invalidate() }
        }
        selector = towards(selector, goalSelector, (360f-offset)*2f/segments.length.toFloat, time)
        r = math.round(towards(r, (nextColor >>> 16) & 0xff, 0xff.toFloat, time))
        g = math.round(towards(g, (nextColor >>>  8) & 0xff, 0xff.toFloat, time))
        b = math.round(towards(b,  nextColor         & 0xff, 0xff.toFloat, time))
      }
    }
  }

  def start(): Unit = {
    setSegments()
    if (thread.isEmpty) {
      val thread = new Thread(this)
      this.thread = Some(thread)
      thread.start()
    }
  }

  def stop(): Unit = {
    for (thread <- this.thread) {
      this.thread = None
      thread.join()
    }
    Settings.save()
  }

  override def run(): Unit = {
    threadID += 1
    val id = threadID
    println(s"Starting ColorView Thread $id...")
    while (thread == Some(Thread.currentThread())) {
      if (threadID != id) {
        MenuActivity.instance.runOnUiThread { stop() }
      }
      val nextUpdate = System.currentTimeMillis
      update(nextUpdate-lastUpdate)
      lastUpdate = nextUpdate
      Thread.sleep(8)
    }
    println(s"Stopping ColorView Thread $id...")
  }
}
