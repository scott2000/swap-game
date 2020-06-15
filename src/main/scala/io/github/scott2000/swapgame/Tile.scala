package io.github.scott2000.swapgame

import android.graphics.{Canvas, Paint}

import io.github.scott2000.bitManager._

object TileType extends Saver[TileType] {
  val values: Array[TileType] = Array(Heaviest, Lightest, Bold, Normal)
  val strokeColor: Int = 0xff202020

  def strokeBetween(amount: Float = 0.5f, redLow: Boolean = false): Int = {
    val a = if (redLow) 1-amount else amount
    if (a <= 0.0f) {
      strokeColor
    } else if (a >= 1.0f) {
      ColorManager.color
    } else {
      (strokeColor & 0xff00ffff) | ((((strokeColor >>> 16) & 0xff)*(1-a) + ((ColorManager.color >>> 16) & 0xff)*a).toInt << 16)
    }
  }

  case object Lightest extends TileType( .22f, false)
  case object Normal   extends TileType( .52f, true)
  case object Bold     extends TileType(  .8f, true)
  case object Heaviest extends TileType( 1.0f, true)

  def randomTile(size: Int = values.length): TileType = values(random.nextInt(size))

  private val saveMask = BitMask(values.length)

  override def read(args: Any*)(implicit reader: BitReader): TileType = values(reader.read(saveMask).toInt)
  override def write(tile: TileType, args: Any*)(implicit writer: BitWriter): Unit = writer.write(Unsigned(values.indexOf(tile)), saveMask)
}

sealed class TileType(val partOfArea: Float, val allowsStatic: Boolean) {
  def calculateStroke(tileSize: Float): Float = ((1-math.sqrt(1-partOfArea))*tileSize/2).toFloat
  val fillColor = 0xffffffff
  val staticFillColor = fillColor//colorAverage(fillColor, ColorManager.color, 1/27f)

  private lazy val _fill: Paint = {
    val fill = new Paint()
    fill.setColor(0xffffffff)
    fill.setStyle(Paint.Style.FILL)
    fill.setAntiAlias(true)
    fill
  }

  private lazy val _stroke: Paint = {
    val stroke = new Paint()
    stroke.setColor(TileType.strokeColor)
    stroke.setStyle(Paint.Style.STROKE)
    stroke.setAntiAlias(true)
    stroke
  }

  def fill: Paint = new Paint(_fill)
  def stroke: Paint = new Paint(_stroke)

  def staticFill: Paint = {
    val fill = new Paint()
    fill.setColor(staticFillColor)
    fill.setStyle(Paint.Style.FILL)
    fill.setAntiAlias(true)
    fill
  }

  def staticStroke: Paint = {
    val stroke = new Paint()
    stroke.setColor(ColorManager.color)
    stroke.setStyle(Paint.Style.STROKE)
    stroke.setAntiAlias(true)
    stroke
  }

  override def toString: String = f"TileType($partOfArea)"
}

object Tile extends Reader[Tile] {
  def create(leveler: Leveler): Tile = {
    val ty = TileType.randomTile(leveler.tiles)
    val static = {
      if (ty.allowsStatic)
        random.nextDouble() < leveler.staticChance
      else
        false
    }
    new Tile(ty, static)
  }

  def read(args: Any*)(implicit reader: BitReader): Tile = Tile(reader.read(TileType), reader.read())
}

case class Tile(tileType: TileType, static: Boolean, animate: Boolean = true) extends Writable {
  import Tile._

  def startAnimation(): Unit = mod = 0.0f

  var mod: Float = if (animate) 0.0f else 1.0f

  def update(time: Long): Unit = mod = towards(mod, 1, 1, time)

  def diagonal: Boolean = false

  def render(canvas: Canvas, position: PositionFraction, tileSize: Float, modifier: Float = mod): Unit = {
    val size = modifier*tileSize
    val strokeWidth = tileType.calculateStroke(size)
    val radius = size/2 - strokeWidth/2
    val stroke = if (static) tileType.staticStroke else tileType.stroke
    stroke.setStrokeWidth(strokeWidth)
    canvas.drawCircle(position.x, position.y, radius, if (static) tileType.staticFill   else tileType.fill)
    canvas.drawCircle(position.x, position.y, radius, stroke)
  }

  def canConnectWith(that: Tile): Boolean = this.tileType == that.tileType
  def canDiagonalWith(that: Tile): Boolean = this.tileType == that.tileType && this.diagonal && that.diagonal
  def canSwapWith(that: Tile): Boolean = this.tileType != that.tileType && !this.static && !that.static
  def strokeColor: Int = if (static) ColorManager.color else TileType.strokeColor

  def value: Int = if (static) 2 else 1

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(tileType, TileType)
    writer.write(static)
  }

  override def toString: String = s"Tile(${TileType.values.indexOf(tileType)}, static=$static)"
}
