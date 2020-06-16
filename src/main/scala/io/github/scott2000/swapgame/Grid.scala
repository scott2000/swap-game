package io.github.scott2000.swapgame

import java.io.File

import org.scaloid.common._

import android.content.Context
import android.graphics.{Canvas, Paint, Rect, RectF, Typeface}
import android.text.TextPaint
import io.github.scott2000.bitManager.{BitReader, BitWriter, Reader, Writable}

import com.google.android.gms.games._
import com.google.android.gms.games.leaderboard.Leaderboards

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max, pow, sqrt, round}

object Grid extends Reader[Grid] {
  def endGame(leveler: Leveler, bestChain: Int, colorBuffer: ArrayBuffer[(UIColor, Long)])(implicit ctx: Context): Unit = {
    MenuActivity.switchTo(GameOver)
    val gameOverLayout = MenuActivity.layout[GameOverLayout](GameOver)
    if (leveler.tutorial) {
      gameOverLayout.endTutorial()
      Settings.disableTutorial()
      Settings.save()
    } else {
      val colors = new ArrayBuffer[UIColor]()
      for ((color, _) <- colorBuffer) {
        colors += color
      }
      gameOverLayout.updateGameStats(Settings.scoreFor(leveler.isChallenge), leveler.score, bestChain, colors.toArray, leveler)
      Settings.addScore(leveler.score, leveler.isChallenge)
      new File(ctx.getFilesDir, saveFile(leveler.isChallenge)).delete()
    }
  }
  var loaded: Option[Boolean] = None
  var tileSize = 96
  val spacingRatio = 36f/96f
  def tileSpacing: Int = tileSize*36/96
  def cropTo: Float = (sqrt(2)*tileSize/2).toFloat
  var fixSize = true
  val maxWidth = 4
  val maxHeight = 5
  val maxSize = 108

  val indent = 64

  def updateTileSize(width: Float, height: Float): Unit = tileSize = if (fixSize) {
    val effectiveWidth = width - indent*2
    val effectiveHeight = height - (indent*2 + swapSize)*2
    val widthSize = effectiveWidth/(spacingRatio + spacingRatio*maxWidth + maxWidth)
    val heightSize = effectiveHeight/(spacingRatio*(maxHeight-1) + maxHeight)
    val targetSize = math.min(widthSize, heightSize)
    math.min(maxSize, targetSize.toInt)
  } else 96

  val hintDelay = 15000
  val hintTime: Int = round(animateTime*10)
  val hintCooldown: Int = hintDelay-hintTime
  val hintEnd: Int = hintDelay+hintTime
  val hintSum: Int = hintEnd+hintCooldown

  val endTime: Int = round(5000+animateTime)

  val saveVersion: Byte = 0

  val typeface = Typeface.create(Typeface.SANS_SERIF, Typeface.NORMAL)
  val typefaceBold = Typeface.create(Typeface.SANS_SERIF, Typeface.BOLD)
  val typefaceMono = Typeface.create(Typeface.MONOSPACE, Typeface.BOLD)
  val titleSize = 96
  val titleSpacing = 36
  val subSize = 48
  val subSpacing = 48

  val swapSize: Int = 52
  val swapSpacing: Int = swapSize*36/96

  private val _textLeft: TextPaint = new TextPaint()
  _textLeft.setAntiAlias(true)
  _textLeft.setTextSize(swapSize)
  _textLeft.setStrokeWidth(3.0f)
  _textLeft.setTypeface(typeface)

  private val _textCenter: TextPaint = new TextPaint(_textLeft)
  _textCenter.setTextAlign(Paint.Align.CENTER)

  private val _textRight: TextPaint = new TextPaint(_textLeft)
  _textRight.setTextAlign(Paint.Align.RIGHT)

  private val _boldLeft = new TextPaint(_textLeft)
  _boldLeft.setTypeface(typefaceBold)

  private val _boldCenter = new TextPaint(_textCenter)
  _boldCenter.setTypeface(typefaceBold)

  private val _boldRight = new TextPaint(_textRight)
  _boldRight.setTypeface(typefaceBold)

  def colorText(paint: TextPaint, color: Int = TileType.strokeColor): TextPaint = {
    paint.setColor(color)
    paint
  }

  def textLeft   = colorText(new TextPaint(_textLeft))
  def textCenter = colorText(new TextPaint(_textCenter))
  def textRight  = colorText(new TextPaint(_textRight))

  def boldLeft   = colorText(new TextPaint(_boldLeft))
  def boldCenter = colorText(new TextPaint(_boldCenter))
  def boldRight  = colorText(new TextPaint(_boldRight))

  def saveFile(challenge: Boolean) = if (challenge) "saveChallenge" else "saveNormal"

  def resetTiles(width: Int, height: Int, function: () => Tile): Array[Array[Option[Tile]]] = {
    val array = new Array[Array[Option[Tile]]](width)
    for (n <- 0 until width) {
      array(n) = new Array[Option[Tile]](height)
      for (n2 <- 0 until height) {
        array(n)(n2) = Some(function())
      }
    }
    array
  }

  def read(args: Any*)(implicit reader: BitReader): Grid = {
    if (args.length < 1) throw new IllegalArgumentException("No context argument provided.")
    implicit val ctx: Context = args(0) match {
      case ctx: Context => ctx
      case _ => throw new IllegalArgumentException("Illegal context argument.")
    }
    val version = reader.readByte()
    val leveler = reader.read(Leveler, version)
    val swaps = reader.readByte()
    var lastSwap: Option[(PositionWhole, PositionWhole)] = None
    if (reader.read()) {
      lastSwap = Some(reader.read(PositionWhole, version), reader.read(PositionWhole, version))
    }
    val colors = reader.readByte()
    val colorBuffer = new ArrayBuffer[(UIColor, Long)]()
    for (_ <- 0 until colors) {
      colorBuffer += ((reader.read(UIColor, version), System.currentTimeMillis-10000))
    }
    val bestChain = reader.readByte()
    new Grid(leveler, swaps, lastSwap, bestChain, colorBuffer)
  }

  def load(challenge: Boolean)(implicit ctx: Context): Unit = {
    if (grid == None || loaded == None || loaded.get != challenge) {
      if (hasSave(challenge) && !Settings.tutorial) {
        val bitReader = new BitReader(ctx.openFileInput(saveFile(challenge)))
        grid = Some(bitReader.read(Grid, ctx))
        bitReader.close()
      } else {
        create(challenge)
      }
    }
    loaded = Some(challenge)
  }

  def create(challenge: Boolean)(implicit ctx: Context): Unit = {
    for (grid <- Grid.grid) {
      grid.willBeDeleted()
    }
    grid = Some(new Grid(Settings.tutorial, challenge))
  }

  def hasSave(challenge: Boolean)(implicit ctx: Context): Boolean = new File(ctx.getFilesDir, saveFile(challenge)).exists()

  var grid: Option[Grid] = None
}

class Grid private (val leveler: Leveler, private var swapPoints: Int,
  private var lastSwap: Option[(PositionWhole, PositionWhole)] = None,
  var bestChain: Int = 0, private var colorBuffer: ArrayBuffer[(UIColor, Long)] = new ArrayBuffer[(UIColor, Long)]())
  (implicit val ctx: Context) extends Writable {

  import Grid._

  def this(leveler: Leveler)(implicit ctx: Context) = this(leveler, leveler.maxSwapPoints)(ctx)
  def this(tutorial: Boolean, challenge: Boolean, size: (Int, Int))(implicit ctx: Context) = this(if (tutorial) new Leveler() else new Leveler(0, size, challenge, Leveler.defaultInitializer))(ctx)
  def this(tutorial: Boolean, challenge: Boolean)(implicit ctx: Context) = this(tutorial, challenge, (4, 5))

  val record: Int = Settings.scoreFor(leveler.isChallenge)

  private var age: Long = 0

  private var startX = 0.0f
  private var startY = 0.0f

  private var scoreParticle: Option[(PositionFraction, Int, Long)] = None

  private var endFlag = false
  private var displaySwaps: Float = swapPoints.toFloat
  private var lastMove: Long = System.currentTimeMillis()
  private var displayScore: Float = leveler.score.toFloat

  private var tileBuffer = new ArrayBuffer[PositionWhole]()

  private val paint = new Paint()
  paint.setAntiAlias(true)

  def pixelWidth: Int = tileSize*leveler.width + tileSpacing*(leveler.width - 1)
  def pixelHeight: Int = tileSize*leveler.height + tileSpacing*(leveler.height - 1)

  def clear(position: PositionWhole): Int = {
    for (e <- leveler.grid(position)) {
      if (swapPoints < leveler.maxSwapPoints) {
        swapPoints += 1
      }
      leveler.clear(position)
      return e.value
    }
    0
  }

  def end(): Unit = {
    if (!endFlag) {
      endFlag = true
      Grid.endGame(leveler, bestChain, colorBuffer)
    }
  }

  def update(time: Long): Unit = {
    this.synchronized {
      age += 1
      leveler.update(time)
      val moveTime = System.currentTimeMillis()-lastMove
      if (!isAlive) {
        end()
      }
      for (position <- leveler.indices; tile <- leveler.grid(position)) {
        tile.update(time)
      }
      displayScore = towards(displayScore, leveler.score, 3.5f, time)
      displaySwaps = towards(displaySwaps, swapPoints, leveler.pointsPerSwap, time)
    }
  }

  def displaySwaps()(implicit canvas: Canvas): Unit = {
    paint.setColor(if (leveler.swaps < 2) TileType.strokeColor else TileType.strokeBetween(displaySwaps-(leveler.pointsPerSwap*2-1), colorLow=true))
    paint.setStrokeWidth(3.0f)
    var n = 0
    var swaps = displaySwaps
    while (n < leveler.swaps) {
      val rect = new RectF(indent+((swapSize+swapSpacing)*n), indent, (indent+swapSize)+((swapSize+swapSpacing)*n), indent+swapSize)
      paint.setStyle(Paint.Style.STROKE)
      canvas.drawOval(rect, paint)
      paint.setStyle(Paint.Style.FILL)
      if (swaps >= leveler.pointsPerSwap) {
        canvas.drawOval(rect, paint)
      } else if (swaps > 0) {
        canvas.drawArc(rect, -90, swaps*360/leveler.pointsPerSwap, true, paint)
      }
      swaps -= leveler.pointsPerSwap
      n += 1
    }
  }

  def displayMoveHint()(implicit canvas: Canvas): Unit = {
    paint.setStrokeWidth(4.0f)
    paint.setStyle(Paint.Style.STROKE)
    val moveTime = (System.currentTimeMillis()-lastMove)%hintSum
    if (moveTime > hintDelay && moveTime < hintEnd) {
      for (move <- recommendMove) {
        val position = toCenter(move)
        paint.setColor((min(min(0x80, max(0, hintEnd-moveTime)), min(0x80, moveTime-hintDelay)).toInt << 24) | (leveler.grid(move).get.strokeColor & 0x00ffffff))
        canvas.drawCircle(position.x, position.y, cropTo, paint)
      }
    }
  }

  def displayScores()(implicit canvas: Canvas): Unit = {
    val score = math.round(math.ceil(displayScore))
    canvas.drawText(s"$score", canvas.getWidth/2, indent+swapSize/2-textCenter.getFontMetrics.ascent/2, textCenter)

    val recordPaint = boldRight
    if (!leveler.tutorial && score > record && record > 0) {
      recordPaint.setColor(TileType.strokeBetween(displayScore-record))
    }
    recordPaint.setTextSize(36)

    val recordText = {
      if (leveler.tutorial)
        "Tutorial"
      else if (record == 0)
        "First Game"
      else if (score > record)
        "New Record"
      else
        s"Record: $record"
    }
    canvas.drawText(recordText, canvas.getWidth-indent, indent+swapSize/2-recordPaint.getFontMetrics.ascent/2, recordPaint)

    for (text <- leveler.displayText) {
      val textSplit = text.split('\n')
      for (index <- textSplit.indices) {
        canvas.drawText(textSplit(index), canvas.getWidth/2, startY-indent+textCenter.getFontMetrics.ascent-(textSplit.length-1-index)*swapSize, textCenter)
      }
    }
    var index = 0
    for ((color, start) <- colorBuffer.reverseIterator) {
      val time = System.currentTimeMillis-start

      val alpha = if (time < animateTime) {
        (time*0xff/animateTime).toInt
      } else if (time < 2000+animateTime) {
        0xff
      } else if (time < 2000+animateTime*2) {
        0xff-((time-2000-animateTime)*0xff/animateTime).toInt
      } else {
        0
      }
      if (alpha != 0) {
        val paint = boldCenter
        paint.setTextSize(48)
        paint.setColor((color.color & 0xffffff) | (alpha << 24))
        val message = {
          if (color == UIColor.Red)
            "Challenge Mode"
          else
            s"${color.name}"
        }
        canvas.drawText(s"$message Unlocked", canvas.getWidth/2, startY-indent/2+paint.getFontMetrics.ascent/2+index*paint.getFontMetrics.top, paint)
        index += 1
      }
    }
  }

  def displayTilesAndLines()(implicit canvas: Canvas, touchLocation: Option[PositionFraction]): Unit = {
    paint.setColor(TileType.strokeColor)
    paint.setStrokeWidth(min(bufferStrokeWidth, tileSize/3))
    paint.setStrokeCap(Paint.Cap.ROUND)
    if (tileBuffer.nonEmpty) {
      for (n <- 1 until tileBuffer.length) {
        paint.setColor(colorAverage(leveler.grid(tileBuffer(n-1)).get.strokeColor, leveler.grid(tileBuffer(n)).get.strokeColor))
        val position0 = toCenter(tileBuffer(n-1))
        val position1 = toCenter(tileBuffer(n))
        canvas.drawLine(position0.x, position0.y, position1.x, position1.y, paint)
      }
    }
    for (position <- leveler.indices if (tileBuffer.isEmpty || touchLocation.isEmpty || position != tileBuffer.last || bufferIsSwap); tile <- leveler.grid(position)) {
      tile.render(canvas, toCenter(position), tileSize)
    }
    for (touch <- touchLocation if !bufferIsSwap && tileBuffer.nonEmpty) {
      for (tile <- leveler.grid(tileBuffer.last)) {
        paint.setColor(tile.strokeColor)
        val position = toCenter(tileBuffer.last)
        canvas.drawLine(position.x, position.y, touch.x, touch.y, paint)
        tile.render(canvas, position, tileSize)
      }
    }
  }

  def displayScoreParticle()(implicit canvas: Canvas): Unit = {
    for ((position, score, startTime) <- scoreParticle) {
      if (leveler.tutorial) {
        return
      }

      val time = System.currentTimeMillis() - startTime
      val fadeStart = animateTime * 2
      if (time > fadeStart + animateTime) {
        return
      }

      val heightOffset = ((tileSize + tileSpacing) / 2.0f) * towards(0.0f, 1.0f, time / animateTime)
      val opacity = {
        if (time < fadeStart) {
          1.0f
        } else {
          towards(1.0f, 0.0f, (time - fadeStart) / animateTime)
        }
      }

      val paint = boldCenter
      paint.setTextSize(math.min((time * time)/(2.0f * animateTime) + 5.0f, math.min(50.0f + score, 100.0f)))

      val text = s"+$score"
      val x = position.x
      val y = position.y + paint.getFontMetrics.ascent/2 - heightOffset

      val borderPaint = new Paint(paint)
      borderPaint.setStyle(Paint.Style.STROKE)
      borderPaint.setStrokeWidth(12)
      borderPaint.setColor((TileType.brightBackground & 0xffffff) | (((1 - math.sqrt(1 - opacity)) * 255).toInt << 24))
      canvas.drawText(text, x, y, borderPaint)

      paint.setColor((ColorManager.color & 0xffffff) | ((opacity * 255).toInt << 24))
      canvas.drawText(text, x, y, paint)
    }
  }

  def displayOn(canvas: Canvas, touchLocation: Option[PositionFraction]): Unit = {
    updateTileSize(canvas.getWidth, canvas.getHeight)

    startX = (canvas.getWidth.toFloat-pixelWidth)/2
    startY = (canvas.getHeight.toFloat-pixelHeight)/2

    implicit val c = canvas
    implicit val t = touchLocation

    this.synchronized {
      displaySwaps()
      displayScores()
      displayMoveHint()
      displayTilesAndLines()
      displayScoreParticle()
    }
  }

  def swaps: Int = swapPoints/leveler.pointsPerSwap

  def diagonalAllowed(nextTile: Option[Tile] = None): Boolean = {
    if (tileBuffer.nonEmpty) {
      for (check <- leveler.grid(tileBuffer.last)) {
        for (next <- nextTile) {
          return check.diagonal && next.diagonal
        }
        return check.diagonal
      }
    }
    false
  }

  def inRangeOfBuffer(position: PositionWhole, tile: Tile): Boolean = {
    tileBuffer.last.distanceTo(position) < (if (diagonalAllowed(Some(tile))) 1.5 else 1.25)
  }

  def touch(positionFraction: PositionFraction): Unit = this.synchronized { touch(fromGrid(positionFraction, diagonalAllowed())) }

  private def touch(position: PositionWhole): Unit = {
    if (!endFlag) {
      val index = tileBuffer.indexOf(position)
      if (index == -1) {
        for (check <- leveler.grid(position)) {
          if (tileBuffer.isEmpty || (inRangeOfBuffer(position, check) && (bufferCanSwap(check, position) || matchesBuffer(check)))) {
            tileBuffer += position
          }
        }
      } else if (index == tileBuffer.length-2) {
        tileBuffer.remove(tileBuffer.length-1)
      }
    }
  }

  def untouch(): Unit = {
    this.synchronized {
      if (!endFlag) {
        val last = tileBuffer.length == 2 && wasLast(tileBuffer(0), tileBuffer(1)) && !leveler.tutorial && (recommendSwap(restrict=true).isDefined || recommendMove.isDefined)
        if (bufferIsSwap && !leveler.isSwapping && (swaps > 0 || last)) {
          lastMove = System.currentTimeMillis()
          lastSwap = if (last) {
            swapPoints += leveler.pointsPerSwap
            None
          } else {
            swapPoints -= leveler.pointsPerSwap
            Some((tileBuffer(0), tileBuffer(1)))
          }
          leveler.swap(tileBuffer(0), tileBuffer(1))
          leveler.tutorialAct()
          save()
        } else if (tileBuffer.length > 2) {
          lastMove = System.currentTimeMillis()
          lastSwap = None
          bestChain = math.max(tileBuffer.length, bestChain)
          val lastTile = tileBuffer.last
          var multiplier = 1
          var increment = 0
          while(tileBuffer.nonEmpty) {
            val amount = clear(tileBuffer.remove(0))
            if (amount == 2) {
              if (multiplier < 10) {
                multiplier += 1
              }
              increment += 1
            } else {
              increment += amount
            }
          }
          val score = increment * multiplier
          if (multiplier > 1) {
            scoreParticle = Some((toCenter(lastTile), score, System.currentTimeMillis()))
          }
          leveler.incrementScore(score)
          leveler.tutorialAct()
          update()
          save()
        }
      }
      tileBuffer.clear()
    }
  }

  def bufferStrokeWidth: Float = {
    if (bufferIsSwap) {
      for (tile0 <- leveler.grid(tileBuffer(0)); tile1 <- leveler.grid(tileBuffer(1))) {
        return (tile0.tileType.calculateStroke(tileSize)+tile1.tileType.calculateStroke(tileSize))/2
      }
    } else if (tileBuffer.nonEmpty) {
      for (position <- tileBuffer; tile <- leveler.grid(position)) {
        return tile.tileType.calculateStroke(tileSize)
      }
    }
    3.0f
  }

  def update(score: Int = leveler.score, chain: Int = bestChain): Unit = {
    for (color <- ColorManager.unlockAll(score, chain)) {
      colorBuffer += ((color, System.currentTimeMillis))
    }
  }

  def matchesBuffer(check: Tile): Boolean = {
    for (tile <- tileBuffer; that <- leveler.grid(tile) if !(that canConnectWith check)) {
      return false
    }
    true
  }

  def swapsBuffer(check: Tile, position: PositionWhole): Boolean = {
    if (swapPoints < leveler.pointsPerSwap && (tileBuffer.isEmpty || leveler.tutorial || !wasLast(position, tileBuffer.last))) {
      return false
    }
    for (tile <- tileBuffer; that <- leveler.grid(tile) if !(that canSwapWith check)) {
      return false
    }
    true
  }

  def bufferCanSwap(tile: Tile, position: PositionWhole): Boolean = tileBuffer.length == 1 && swapsBuffer(tile, position)

  def bufferIsSwap: Boolean = {
    if (tileBuffer.length == 2) {
      leveler.grid(tileBuffer(0)).get canSwapWith leveler.grid(tileBuffer(1)).get
    } else false
  }

  def toOrigin(position: PositionWhole): PositionFraction = {
    val positionFloat = leveler.getExactPosition(position)
    PositionFraction(startX+positionFloat.x*(tileSize+tileSpacing), startY+positionFloat.y*(tileSize+tileSpacing))
  }

  def toCenter(position: PositionWhole): PositionFraction = {
    val position0 = toOrigin(position)
    PositionFraction(position0.x + (tileSize.toFloat/2), position0.y + (tileSize.toFloat/2))
  }

  def fromGrid(position: PositionFraction, crop: Boolean = false): PositionWhole = {
    val (fx, fy) = ((position.x-startX+tileSpacing/2)/(tileSize+tileSpacing), (position.y-startY+tileSpacing/2)/(tileSize+tileSpacing))
    if (fx < 0 || fy < 0) return PositionWhole(-1, -1)
    val p = PositionWhole(fx.toInt, fy.toInt)
    if (crop) {
      val p2 = toCenter(p)
      if (sqrt(pow(p2.x-position.x, 2) + pow(p2.y-position.y, 2)) > cropTo) return PositionWhole(-1, -1)
    }
    p
  }

  def recommendMove: Option[PositionWhole] = {
    for (position <- leveler.indices; check <- leveler.grid(position)) {
      var tile = 0
      for ((xd, yd) <- {
        if (!check.diagonal) Array((-1, 0), (1, 0), (0, -1), (0, 1))
        else Array((-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1))
      }) {
        val position2 = PositionWhole(position.x+xd, position.y+yd)
        for (that <- leveler.grid(position2) if that.canConnectWith(check)) {
          val dist = position.distanceTo(position2)
          if (dist < 1.25 || that.diagonal) {
            tile += 1
            if (tile >= 2) return Some(position)
          }
        }
      }
    }
    None
  }

  def recommendSwap: Option[PositionWhole] = recommendSwap(restrict=false)

  def recommendSwap(restrict: Boolean): Option[PositionWhole] = {
    for (position <- leveler.indices; check <- leveler.grid(position)) {
      if (!check.static) {
        for ((xd, yd) <- Array((-1, 0), (1, 0), (0, -1), (0, 1))) {
          val position2 = PositionWhole(position.x+xd, position.y+yd)
          for (that <- leveler.grid(position2)) {
            if (that.canSwapWith(check) && (!restrict || !wasLast(position, position2))) {
              return Some(position)
            }
          }
        }
      }
    }
    None
  }

  def wasLast(a: PositionWhole, b: PositionWhole): Boolean = {
    for (swap <- lastSwap) {
      val (c, d) = swap
      if ((a == c || b == c) && (a == d || b == d)) {
        return true
      }
    }
    false
  }

  def isAnimated: Boolean = displayScore < leveler.score || displaySwaps != swapPoints || leveler.isSwapping
  def visibleSwaps: Boolean = displaySwaps >= leveler.pointsPerSwap || swapPoints >= leveler.pointsPerSwap
  def canMove: Boolean = recommendMove.isDefined
  def canSwap: Boolean = visibleSwaps && recommendSwap.isDefined

  def isAlive: Boolean = !leveler.shouldExit && (isAnimated || canMove || canSwap)

  def outOfBounds(position: PositionFraction): Boolean = !leveler.inBounds(fromGrid(position, crop=false))
  def writeWith(writer: BitWriter): Unit = {
    writer.write(Grid.saveVersion)
    writer.write(leveler)
    writer.write(swapPoints.toByte)
    writer.write(lastSwap.isDefined)
    for (last <- lastSwap) {
      writer.write(last._1)
      writer.write(last._2)
    }
    writer.write(colorBuffer.size.toByte)
    for ((color, _) <- colorBuffer) {
      writer.write(color)
    }
    writer.write(bestChain.toByte)
  }

  def willBeDeleted(): Unit = {
    Settings.addScore(leveler.score, leveler.isChallenge)
  }

  def save(): Unit = if (!endFlag && !leveler.tutorial) {
    val bitWriter = new BitWriter(ctx.openFileOutput(saveFile(leveler.isChallenge), Context.MODE_PRIVATE))
    bitWriter.write(this)
    bitWriter.close()
  }
}
