package io.github.scott2000.swapgame

import io.github.scott2000.bitManager.{BitReader, BitWriter, Writable, Reader}

import scala.collection.mutable.ArrayBuffer

object Leveler extends Reader[Leveler] {
  val staticChanceChallenge: Float = 0.53f
  val staticChanceMax: Float = 0.5f
  val staticChanceInflection: Float = 1000.0f
  val staticChanceRate: Float = 500.0f

  def defaultInitializer(leveler: Leveler, position: PositionWhole): Option[Tile] = Some(Tile.create(leveler))

  def read(args: Any*)(implicit reader: BitReader): Leveler = {
    val score = reader.readInt()
    val width = reader.readByte()
    val height = reader.readByte()
    val isChallenge = reader.read()
    new Leveler(score, (width, height), isChallenge, (_, _) => Some(reader.read(Tile)))
  }

  object TutorialController {
    import TileType._

    def template(array: Array[Option[Tile]]): (Leveler, PositionWhole) => Option[Tile] =
      (leveler: Leveler, position: PositionWhole) => {
        val tileOption = array(position.x+position.y*3)
        for (tile <- tileOption) tile.startAnimation()
        tileOption
      }

    lazy val emptyList: Array[Option[Tile]] = Array(
      None, None, None,
      None, None, None,
      None, None, None
    )

    lazy val stageList: Array[(Array[Option[Tile]], Array[String])] = Array(
      (Array(
        None,                        None,                        None,
        Some(Tile(    Bold, false)), Some(Tile(    Bold, false)), Some(Tile(    Bold, false)),
        None,                        None,                        None
      ), Array("Connect at least three\nsimilar tiles to earn points.\nYou lose if you can't do anything.")),
      (Array(
        None,                        None,                        None,
        Some(Tile(Lightest, false)), Some(Tile(  Normal, false)), Some(Tile(  Normal, false)),
        Some(Tile(  Normal, false)), None,                        None
      ), Array("Swap different tiles\nto make longer chains.", "Swaps are somewhat limited but\nrecharge after connecting tiles.")),
      (Array(
        None,                        Some(Tile(Bold, false)),     Some(Tile(Normal, false)),
        Some(Tile  (Normal, false)), Some(Tile(Normal,  true)),   Some(Tile(Bold, true)),
        None,                        None,                        None
      ), Array(s"${ColorManager.uiColor.name} tiles can't be swapped\nbut give extra points.", "The goal is to get the best score\nand to unlock new colors."))
    )
  }

  class TutorialController(leveler: Leveler) {
    import TutorialController._

    private var stage = -1
    private var part: Int = _
    private var array: Array[Option[Tile]] = _
    private var strings: Array[String] = _
    next()

    def act(): Unit = {
      part += 1
      if (part >= strings.length) {
        next()
      }
    }

    private def next(): Unit = {
      stage += 1
      if (!isDone) {
        part = 0
        array = stageList(stage)._1
        strings = stageList(stage)._2
        leveler.setGrid(TutorialController.template(array))
      } else {
        leveler.setGrid(TutorialController.template(emptyList))
        leveler.endTutorial()
      }
    }

    def displayText: Option[String] = if (isDone) None else Some(stageList(stage)._2(part))

    def isDone: Boolean = stage >= stageList.length
  }
}

class Leveler private (val width: Int, val height: Int, val isChallenge: Boolean) extends Writable {
  import Leveler._

  var tutorialController: Option[TutorialController] = None
  var _exit = false

  def tutorial: Boolean = tutorialController.isDefined || _exit
  def displayText: Option[String] = {
    for (tutorial <- tutorialController) {
      return tutorial.displayText
    }
    None
  }
  def tutorialAct(): Unit = for (tutorial <- tutorialController) tutorial.act()

  def endTutorial(): Unit = {
    if (tutorialController.nonEmpty) {
      tutorialController = None
      _exit = true
    }
  }

  private var _score = 0

  def score: Int = _score
  def incrementScore(increment: Int = 1): Unit = _score += increment
  def resetScore(): Unit = _score = 0

  def shouldExit: Boolean = _exit

  def this(score: Int, size: (Int, Int), isChallenge: Boolean, gridInitializer: (Leveler, PositionWhole) => Option[Tile]) = {
    this(size._1, size._2, isChallenge)
    _score = score
    setGrid(gridInitializer)
  }

  def this() = {
    this(3, 3, false)
    _swaps = 1
    tutorialController = Some(new TutorialController(this))
  }

  private var _grid: Option[Array[Option[Tile]]] = None
  private var swapBuffer: Option[(Tile, Tile, PositionWhole, PositionWhole, Long)] = None

  private def getGrid: Array[Option[Tile]] = {
    if (_grid.isEmpty) {
      setGrid()
    }
    _grid.get
  }

  def isSwapping: Boolean = swapBuffer.isDefined

  def grid(position: PositionWhole, grid: Array[Option[Tile]] = getGrid): Option[Tile] = {
    if (!inBounds(position)) return None
    val index = toGridIndex(position)
    for (swap <- swapBuffer) {
      val (tile0, tile1, position0, position1, _) = swap
      if (position == position0) return Some(tile0)
      else if (position == position1) return Some(tile1)
    }
    getGrid(index)
  }

  def updateGrid(position: PositionWhole, tile: Option[Tile], grid: Array[Option[Tile]] = getGrid): Unit = {
    if (inBounds(position)) {
      for (swap <- swapBuffer; tile2 <- tile) {
        val (tile0, tile1, position0, position1, start) = swap
        if (position == position0) {
          swapBuffer = Some((tile2, tile1, position0, position1, start))
          return
        } else if (position == position1) {
          swapBuffer = Some((tile0, tile2, position0, position1, start))
          return
        }
      }
      grid(toGridIndex(position)) = tile
    }
  }

  def clear(position: PositionWhole, initializer: (Leveler, PositionWhole) => Option[Tile] = defaultInitializer, grid: Array[Option[Tile]] = getGrid): Unit =
    updateGrid(position, initializer(this, position), grid)

  def swap(position0: PositionWhole, position1: PositionWhole): Unit = {
    if (!isSwapping) {
      swapBuffer = Some(grid(position0).get, grid(position1).get, position1, position0, System.currentTimeMillis())
      getGrid(toGridIndex(position0)) = None
      getGrid(toGridIndex(position1)) = None
    }
  }

  def getExactPosition(position: PositionWhole): PositionFraction = {
    for (swap <- swapBuffer) {
      val (_, _, position0, position1, start) = swap
      val mod = (System.currentTimeMillis()-start).toFloat/animateTime
      if (position == position0) return position1.to(position0, mod)
      else if (position == position1) return position0.to(position1, mod)
    }
    PositionFraction(position.x.toFloat, position.y.toFloat)
  }

  def inBounds(position: PositionWhole): Boolean = position.x >= 0 && position.x < width && position.y >= 0 && position.y < height

  def setGrid(initializer: (Leveler, PositionWhole) => Option[Tile] = defaultInitializer): Unit = {
    _grid = Some(new Array[Option[Tile]](width*height))
    for (x <- 0 until width; y <- 0 until height) {
      val position = PositionWhole(x, y)
      updateGrid(position, initializer(this, position))
    }
  }

  private def toGridIndex(position: PositionWhole): Int = position.x+position.y*width
  private def fromGridIndex(index: Int): PositionWhole = PositionWhole(index%width, index/width)

  def staticChance: Float = {
    val wave = -0.1 * math.sin(math.Pi * score / staticChanceRate)
    if (isChallenge)
      (staticChanceChallenge + wave).toFloat
    else {
      val denom = 1 + math.exp(-(score - staticChanceInflection) / staticChanceRate)
      ((staticChanceMax + wave) / denom).toFloat
    }
  }

  lazy val indices: Array[PositionWhole] = {
    val array = new ArrayBuffer[PositionWhole]
    for (x <- 0 until width; y <- 0 until height) {
      array += PositionWhole(x, y)
    }
    array.toArray.sortWith((a: PositionWhole, b: PositionWhole) => {
      def c(a: PositionWhole) = math.abs(a.x-(width-1)/2f)+math.abs(a.y-(height-1)/2f)
      val ca = c(a)
      val cb = c(b)
      if (ca < cb)
        true
      else if (ca > cb)
        false
      else
        if (a.x < b.x)
          true
        else if (a.x > b.x)
          false
        else
          if (a.y < b.y)
            true
          else
            false
    })
  }

  val tiles: Int = TileType.values.length
  private var _swaps = 3
  def swaps: Int = _swaps

  def pointsPerSwap: Int = if (tutorial) 3 else tiles
  def maxSwapPoints: Int = swaps*pointsPerSwap

  def update(time: Long): Unit = {
    for (swap <- swapBuffer) {
      val (tile0, tile1, position0, position1, start) = swap
      if (System.currentTimeMillis()-start >= animateTime) {
        getGrid(toGridIndex(position0)) = Some(tile0)
        getGrid(toGridIndex(position1)) = Some(tile1)
        swapBuffer = None
      }
    }
  }

  override def writeWith(writer: BitWriter): Unit = {
    writer.write(_score)
    writer.write(width.toByte)
    writer.write(height.toByte)
    writer.write(isChallenge)
    for (x <- 0 until width; y <- 0 until height) {
      writer.write(grid(PositionWhole(x, y)).get)
    }
  }

  override def toString: String = s"Leveler($width, $height)"
}
