package io.github.scott2000.swapgame

import org.scaloid.common._
import android.content.Context
import android.graphics.Paint
import android.view.Gravity
import android.widget.LinearLayout

object MenuLayout {
  def title[LP <: ViewGroupLayoutParams[_, STextView]](text: String)(implicit ctx: Context, defaultLayoutParam: (STextView) ⇒ LP): STextView = {
    formatTitle(STextView(text))
  }

  def clickableTitle[LP <: ViewGroupLayoutParams[_, STextView]](text: String, callback: ViewOnClickListener)(implicit ctx: Context, defaultLayoutParam: (STextView) ⇒ LP): STextView = {
    formatTitle(STextView(text, callback))
  }

  def formatTitle(textView: STextView): STextView = {
    textView
      .textColor(TileType.strokeColor)
      .textSize(Grid.titleSize)
      .typeface(Grid.typeface)
      .padding(Grid.tileSpacing)
  }
}

trait MenuLayout extends SLinearLayout {
  def uuid: Option[Int] = None

  gravity = Gravity.CENTER
  orientation = LinearLayout.VERTICAL

  private var _set = false

  def refresh(): Unit = {}
  def clean(): Unit = {}

  def changeAPI(): Unit = {}

  final def set(id: Option[Int] = None): Unit = set(uuid == id)
  final def set(visible: Boolean): Unit = {
    require(!_set, "The layout has already been set.")
    _set = true
    if (visible) {
      setVisible()
    } else {
      setHidden()
    }
  }

  protected def setVisible(): Unit = {}
  protected def setHidden(): Unit

  protected def showFrom(previous: MenuLayout): Unit
  protected def hideForAnd(next: MenuLayout, action: => Unit): Unit

  protected def isReady: Boolean = true

  def back(): Boolean = false
  def longBack(): Boolean = false

  final def to(layout: MenuLayout): Boolean = {
    require(_set,        "The current layout was never set to an initial state.")
    require(layout._set, "The target layout was never set to an initial state.")
    if (isReady && layout.isReady) {
      MenuActivity.instance.runOnUiThread {
        hideForAnd(layout, {
          clean()
          layout.refresh()
          layout.showFrom(this)
        })
      }
      true
    } else {
      false
    }
  }
}
