// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app.interfacetab

import java.awt.{ Cursor, Dimension, Graphics, Point, Rectangle }
import java.awt.event.{ ActionEvent, InputEvent, MouseAdapter, MouseEvent, MouseListener,  MouseMotionAdapter,
                        MouseMotionListener }
import javax.swing.{ AbstractAction, JComponent, JLayeredPane, JPanel }

import org.nlogo.app.common.Events.WidgetSelectedEvent
import org.nlogo.awt.{ Coordinates, Mouse }
import org.nlogo.core.I18N
import org.nlogo.swing.{ MenuItem, PopupMenu, WrappingPopupMenu, Utils }
import org.nlogo.theme.{ InterfaceColors, ThemeSync }
import org.nlogo.window.{ InterfaceMode, MouseMode, ViewWidget, Widget, WidgetWrapperInterface }
import org.nlogo.window.Events.{ DirtyEvent, EditWidgetEvent, ExportWidgetEvent, WidgetForegroundedEvent }

object WidgetWrapper {
  private val HandleSize = 7

  // the space around each handle that counts as grabbing the handle (Isaac B 7/11/25)
  private val GrabBuffer = 5

  val BorderSize = HandleSize + GrabBuffer

  private val MinWidgetWidth = 12
  private val MinWidgetHeight = 12
}

// public for widget extension - ST 6/12/08
class WidgetWrapper(val widget: Widget, val interfacePanel: WidgetPanel)
  extends JLayeredPane
  with WidgetWrapperInterface
  with MouseListener
  with MouseMotionListener
  with WidgetForegroundedEvent.Handler
  with ThemeSync {

  import WidgetWrapper._

  private var _isNew = false
  private var _selected = false
  private var _isForeground = false
  private var highlighted = false
  private var dragging = false
  private var placing = false
  private var mouseMode = MouseMode.IDLE
  private var startPressX = 0
  private var startPressY = 0
  private var constrainToHorizontal = false
  private var constrainToVertical = false

  private var startBoundsUnselected: Option[Rectangle] = None

  private var snapped = false

  private val glass = new JComponent {}

  glass.setOpaque(false)
  glass.addMouseListener(this)
  glass.addMouseMotionListener(this)

  private val shadowPane = new ShadowPane

  var originalBounds: Rectangle = null

  // this is for notes, when the bg is transparent we don't want to see the
  // white widget wrapper.  I don't know why setting the background to a
  // transparent color doesn't work.  but it doesn't ev 6/8/07
  setOpaque(false)

  setBackground(widget.getBackground)
  setLayout(null)

  add(glass, JLayeredPane.DRAG_LAYER)
  add(widget)
  add(shadowPane, JLayeredPane.PALETTE_LAYER)

  doLayout()

  // don't let mouse events get through to the InterfacePanel
  // (is there a more elegant way to do this?) - ST 8/9/03
  addMouseListener(new MouseAdapter {})
  addMouseMotionListener(new MouseMotionAdapter {})

  widget.addPopupListeners(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      if (e.isPopupTrigger)
        doPopup(e)
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      if (e.isPopupTrigger)
        doPopup(e)
    }
  })

  def isNew: Boolean =
    _isNew

  def isNew(isNew: Boolean): Unit = {
    _isNew = isNew
  }

  override def isValidateRoot: Boolean =
    true

  def selected: Boolean =
    _selected

  def selected(selected: Boolean, temporary: Boolean = false): Unit = {
    if (_selected != selected) {
      _selected = selected
      highlighted = selected

      if (selected) {
        setBounds(getX - BorderSize, getY - BorderSize,
                  getWidth + BorderSize + BorderSize,
                  getHeight + BorderSize + BorderSize)
      } else {
        isForeground(false)
        setBounds(getX + BorderSize, getY + BorderSize,
                  getWidth - BorderSize - BorderSize,
                  getHeight - BorderSize - BorderSize)
      }

      revalidate()
      repaint()

      if (!temporary)
        new WidgetSelectedEvent(widget, selected).raise(this)
    }
  }

  def setHighlight(on: Boolean): Unit = {
    if (highlighted != on) {
      highlighted = on

      revalidate()
      repaint()
    }
  }

  private def revalidateInterfacePanel(): Unit = {
    if (interfacePanel != null)
      interfacePanel.revalidate()
  }

  def snapLocation(x: Int, y: Int): Unit = {
    if (snapped) {
      setLocation(snapToGrid(x), snapToGrid(y))
    } else {
      val anchorDist = (x - originalBounds.x) * (x - originalBounds.x) + (y - originalBounds.y) * (y - originalBounds.y)

      val xSnap = snapToGrid(x)
      val ySnap = snapToGrid(y)

      val snapDist = (x - xSnap) * (x - xSnap) + (y - ySnap) * (y - ySnap)

      if (anchorDist <= snapDist) {
        setLocation(originalBounds.x, originalBounds.y)
      } else {
        setLocation(xSnap, ySnap)

        snapped = true
      }
    }
  }

  override def setBounds(r: Rectangle): Unit = {
    setBounds(r.x, r.y, r.width, r.height)
  }

  override def setBounds(x: Int, y: Int, width: Int, height: Int): Unit = {
    val sizeChanged = getWidth != width || getHeight != height

    super.setBounds(x, y, width, height)

    if (sizeChanged) {
      doLayout()
      revalidateInterfacePanel()
    }
  }

  def isForeground: Boolean =
    _isForeground

  def isForeground(isForeground: Boolean): Unit = {
    if (_isForeground != isForeground) {
      _isForeground = isForeground

      repaint()
    }
  }

  def foreground(): Unit = {
    if (!isForeground) {
      isForeground(true)

      new WidgetForegroundedEvent(widget).raise(this)
    }
  }

  def setPlacing(value: Boolean): Unit = {
    placing = value
  }

  private def addWrapperBorder(dim: Dimension): Dimension = {
    // some widgets have no max size.
    // Adding the border dimensions to that results in another null -- CLB
    if (dim == null)
      return null

    val newDim =
      getParent match {
        case wp: WidgetPanel if !widget.isNote => wp.zoomer.zoomSize(dim)
        case _ => dim
      }

    if (selected) {
      new Dimension(newDim.width + BorderSize + BorderSize, newDim.height + BorderSize + BorderSize)
    } else {
      newDim
    }
  }

  override def getMinimumSize: Dimension =
    addWrapperBorder(widget.getMinimumSize)

  override def getPreferredSize: Dimension = {
    addWrapperBorder(
      if (widget.isNote) {
        widget.getPreferredSize
      } else {
        widget.getUnzoomedPreferredSize
      }
    )
  }

  override def getMaximumSize: Dimension =
    addWrapperBorder(widget.getMaximumSize)

  override def doLayout(): Unit = {
    if (selected) {
      widget.setBounds(BorderSize, BorderSize, getWidth - BorderSize - BorderSize,
                       getHeight - BorderSize - BorderSize)
    } else {
      widget.setBounds(0, 0, getWidth, getHeight)
    }

    widget.validate()

    glass.setBounds(0, 0, getWidth, getHeight)
    glass.setVisible(selected)

    shadowPane.setBounds(0, 0, getWidth, getHeight)
  }

  def widgetX: Int =
    getX + widget.getX

  def widgetY: Int =
    getY + widget.getY

  def widgetWidth: Int =
    widget.getWidth

  def widgetHeight: Int =
    widget.getHeight

  def widgetBounds: Rectangle =
    new Rectangle(getX + widget.getX, getY + widget.getY, widget.getWidth, widget.getHeight)

  def doResize(x: Int, y: Int, ignoreSnap: Boolean): Unit = {
    /* x and y represent the distance from the original click and the dragged cursor position,
        so the widget can resize based on the position of the cursor. Interestingly, the
        x and y can be negative since the difference is calculated from the coordinates.
        Also, the bounds.x and bounds.width refer to the original bounds before resizing began
        and will be updated through the new rectangle initialized below. CBR 01/09/19.
      */
    val bounds = new Rectangle(originalBounds)

    mouseMode match {
      case MouseMode.NW =>
        val newY = y.max(-bounds.y)
        val newX = x.max(-bounds.x)

        bounds.x += newX
        bounds.width -= newX
        bounds.y += newY
        bounds.height -= newY

      case MouseMode.NE =>
        val newY = y.max(-bounds.y)
        val newX = x.max(-bounds.x - bounds.width)

        bounds.width += newX
        bounds.y += newY
        bounds.height -= newY

      case MouseMode.SW =>
        val newX = x.max(-bounds.x)

        bounds.x += newX
        bounds.width -= newX
        bounds.height += y

      case MouseMode.W =>
        val newX = x.max(-bounds.x)

        bounds.x += newX
        bounds.width -= newX

      case MouseMode.SE =>
        bounds.width += x.max(-bounds.x - bounds.width)
        bounds.height += y

      case MouseMode.E =>
        bounds.width += x.max(-bounds.x - bounds.width)

      case MouseMode.S =>
        bounds.height += y

      case MouseMode.N =>
        val newY = y.max(-bounds.y)

        bounds.y += newY
        bounds.height -= newY

      case _ => throw new IllegalStateException
    }

    if (interfacePanel.workspace.snapOn && !ignoreSnap)
      enforceGridSnapSize(bounds)

    enforceMinimumSize(bounds)
    enforceMaximumSize(bounds)

    setBounds(widget.constrainDrag(bounds, originalBounds, mouseMode))
  }

  def snapToGrid(value: Int): Int = {
    if (interfacePanel.workspace.snapOn) {
      interfacePanel.snapToGrid(value)
    } else {
      value
    }
  }

  // GrabBuffer is added to BorderSize again in all of these checks because for positioning and rendering
  // purposes, BorderSize only contains the outer portion of GrabBuffer, but when checking for mouse clicks
  // we also want to have some GrabBuffer on the inside over top of the widget itself (Isaac B 7/11/25)
  private def getHandle(x: Int, y: Int): Option[MouseMode] = {
    if (x < BorderSize + GrabBuffer) {
      if (y < BorderSize + GrabBuffer) {
        Some(MouseMode.NW)
      } else if (y > getHeight - BorderSize - GrabBuffer) {
        Some(MouseMode.SW)
      } else if (y <= (getHeight + HandleSize) / 2 + GrabBuffer && y >= (getHeight - HandleSize) / 2 - GrabBuffer) {
        Some(MouseMode.W)
      } else {
        None
      }
    } else if (x > getWidth - BorderSize - GrabBuffer) {
      if (y < BorderSize + GrabBuffer) {
        Some(MouseMode.NE)
      } else if (y > getHeight - BorderSize - GrabBuffer) {
        Some(MouseMode.SE)
      } else if (y <= (getHeight + HandleSize) / 2 + GrabBuffer && y >= (getHeight - HandleSize) / 2 - GrabBuffer) {
        Some(MouseMode.E)
      } else {
        None
      }
    } else if (y > getHeight - BorderSize - GrabBuffer) {
      if (x <= (getWidth + HandleSize) / 2 + GrabBuffer && x >= (getWidth - HandleSize) / 2 - GrabBuffer) {
        Some(MouseMode.S)
      } else {
        None
      }
    } else if (y < BorderSize + GrabBuffer) {
      if (x <= (getWidth + HandleSize) / 2 + GrabBuffer && x >= (getWidth - HandleSize) / 2 - GrabBuffer) {
        Some(MouseMode.N)
      } else {
        None
      }
    } else {
      None
    }
  }

  def mouseMoved(e: MouseEvent): Unit = {
    interfacePanel.setCursor(
      getHandle(e.getX, e.getY) match {
        case Some(MouseMode.N) =>
          Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)

        case Some(MouseMode.NE) =>
          Cursor.getPredefinedCursor(Cursor.NE_RESIZE_CURSOR)

        case Some(MouseMode.E) =>
          Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR)

        case Some(MouseMode.SE) =>
          Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR)

        case Some(MouseMode.S) =>
          Cursor.getPredefinedCursor(Cursor.S_RESIZE_CURSOR)

        case Some(MouseMode.SW) =>
          Cursor.getPredefinedCursor(Cursor.SW_RESIZE_CURSOR)

        case Some(MouseMode.W) =>
          Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)

        case Some(MouseMode.NW) =>
          Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR)

        case _ =>
          Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR)
      }
    )
  }

  def mouseClicked(e: MouseEvent): Unit = {}
  def mouseEntered(e: MouseEvent): Unit = {}
  def mouseExited(e: MouseEvent): Unit = {}

  def mousePressed(e: MouseEvent): Unit = {
    if (e.isPopupTrigger && mouseMode != MouseMode.DRAG) {
      doPopup(e)

      return
    }

    if (!Mouse.hasButton1(e))
      return

    foreground()

    if (e.getClickCount == 2) {
      widget.getEditable.foreach(new EditWidgetEvent(_).raise(this))

      return
    }

    val x = e.getX
    val y = e.getY

    val p = Coordinates.convertPointToScreen(e.getPoint, this)

    startPressX = p.x
    startPressY = p.y

    mouseMode = getHandle(x, y).getOrElse(MouseMode.DRAG)

    if (mouseMode == MouseMode.DRAG) {
      interfacePanel.aboutToDragSelectedWidgets(this, startPressX, startPressY)
    } else {
      interfacePanel.beginResizeWidget(this)
      aboutToDrag(startPressX, startPressY)
    }
  }

  def aboutToDrag(startX: Int, startY: Int): Unit = {
    startPressX = startX
    startPressY = startY
    selected(false, true) // true = change is temporary, don't raise events
    originalBounds = getBounds()
    startBoundsUnselected = Option(getUnselectedBounds)
    snapped = false
    dragging = true
  }

  def mouseDragged(e: MouseEvent): Unit = {
    val p = Coordinates.convertPointToScreen(e.getPoint, this)

    if (mouseMode == MouseMode.DRAG) {
      if ((e.getModifiersEx & InputEvent.SHIFT_DOWN_MASK) == 0) {
        constrainToHorizontal = false
        constrainToVertical = false
      } else {
        if (!constrainToHorizontal && !constrainToVertical &&
            (p.x - startPressX).abs > (p.y - startPressY).abs) {
          constrainToHorizontal = true
        } else {
          constrainToVertical = true
        }

        if (constrainToHorizontal) {
          p.y = startPressY
        } else if (constrainToVertical) {
          p.x = startPressX
        }
      }

      interfacePanel.dragSelectedWidgets(p.x - startPressX, p.y - startPressY)
    } else if (mouseMode != MouseMode.IDLE) {
      doResize(p.x - startPressX, p.y - startPressY, Mouse.hasCtrl(e))
    }
  }

  def mouseReleased(e: MouseEvent): Unit = {
    if (e.isPopupTrigger && mouseMode != MouseMode.DRAG) {
      doPopup(e)
    } else if (Mouse.hasButton1(e)) {
      selected(true)

      if (mouseMode == MouseMode.DRAG) {
        WidgetActions.moveSelectedWidgets(interfacePanel)
      } else if (mouseMode != MouseMode.IDLE) {
        interfacePanel.endResizeWidget()
        WidgetActions.resizeWidget(this)
      }

      mouseMode = MouseMode.IDLE
    }
  }

  def doDrop(): Unit = {
    selected(true, true) // 2nd true = change was temporary

    if (!startBoundsUnselected.contains(getUnselectedBounds))
      new DirtyEvent(None).raise(this)

    getParent.asInstanceOf[WidgetPanel].zoomer.updateZoomInfo(widget)

    dragging = false
  }

  private def enforceMinimumSize(r: Rectangle): Unit = {
    if (widget != null) {
      var minWidgetSize = widget.getMinimumSize

      getParent match {
        case wp: WidgetPanel => wp.zoomer.zoomSize(minWidgetSize)
        case _ =>
      }

      minWidgetSize = new Dimension(minWidgetSize.width.max(MinWidgetWidth), minWidgetSize.height.max(MinWidgetHeight))

      mouseMode match {
        case MouseMode.S =>
          if (r.height < minWidgetSize.height)
            r.height = minWidgetSize.height

        case MouseMode.SW =>
          if (r.width < minWidgetSize.width) {
            r.x -= minWidgetSize.width - r.width
            r.width = minWidgetSize.width
          }

          if (r.height < minWidgetSize.height)
            r.height = minWidgetSize.height

        case MouseMode.SE =>
          if (r.width < minWidgetSize.width)
            r.width = minWidgetSize.width

          if (r.height < minWidgetSize.height)
            r.height = minWidgetSize.height

        case MouseMode.E =>
          if (r.width < minWidgetSize.width)
            r.width = minWidgetSize.width

        case MouseMode.NW =>
          if (r.width < minWidgetSize.width) {
            r.x -= minWidgetSize.width - r.width
            r.width = minWidgetSize.width
          }

          if (r.height < minWidgetSize.height) {
            r.y -= minWidgetSize.height - r.height
            r.height = minWidgetSize.height
          }

        case MouseMode.W =>
          if (r.width < minWidgetSize.width) {
            r.x -= minWidgetSize.width - r.width
            r.width = minWidgetSize.width
          }

        case MouseMode.NE =>
          if (r.width < minWidgetSize.width)
            r.width = minWidgetSize.width

          if (r.height < minWidgetSize.height) {
            r.y -= minWidgetSize.height - r.height
            r.height = minWidgetSize.height
          }

        case MouseMode.N =>
          if (r.height < minWidgetSize.height) {
            r.y -= minWidgetSize.height - r.height
            r.height = minWidgetSize.height
          }

        case _ => throw new IllegalStateException
      }
    }
  }

  private def enforceMaximumSize(r: Rectangle): Unit = {
    if (widget != null) {
      val maxWidgetSize = widget.getMaximumSize

      if (maxWidgetSize == null)
        return

      if (maxWidgetSize.height <= 0)
        maxWidgetSize.height = 10000

      if (maxWidgetSize.width <= 0)
        maxWidgetSize.width = 10000

      getParent match {
        case wp: WidgetPanel => wp.zoomer.zoomSize(maxWidgetSize)
        case _ =>
      }

      mouseMode match {
        case MouseMode.S =>
          if (r.height > maxWidgetSize.height)
            r.height = maxWidgetSize.height

        case MouseMode.SW =>
          if (r.width > maxWidgetSize.width)
            r.width = maxWidgetSize.width

          if (r.height > maxWidgetSize.height)
            r.height = maxWidgetSize.height

        case MouseMode.SE =>
          if (r.width > maxWidgetSize.width) {
            r.width = maxWidgetSize.width
            r.x = getX + getWidth - r.width
          }

          if (r.height > maxWidgetSize.height)
            r.height = maxWidgetSize.height

        case MouseMode.E =>
          if (r.width > maxWidgetSize.width) {
            r.width = maxWidgetSize.width
            r.x = getX + getWidth - r.width
          }

        case MouseMode.NW =>
          if (r.width > maxWidgetSize.width)
            r.width = maxWidgetSize.width

          if (r.height > maxWidgetSize.height) {
            r.height = maxWidgetSize.height
            r.y = getY + getHeight - r.height
          }

        case MouseMode.W =>
          if (r.width > maxWidgetSize.width)
            r.width = maxWidgetSize.width

        case MouseMode.NE =>
          if (r.width > maxWidgetSize.width) {
            r.width = maxWidgetSize.width
            r.x = getX + getWidth - r.width
          }

          if (r.height > maxWidgetSize.height) {
            r.height = maxWidgetSize.height
            r.y = getY + getHeight - r.height
          }

        case MouseMode.N =>
          if (r.height > maxWidgetSize.height) {
            r.height = maxWidgetSize.height
            r.y = getY + getHeight - r.height
          }

        case _ => throw new IllegalStateException
      }
    }
  }

  private def enforceGridSnapSize(r: Rectangle): Unit = {
    if (widget != null) {
      val newWidth = interfacePanel.snapToGrid(r.width)
      val newHeight = interfacePanel.snapToGrid(r.height)

      mouseMode match {
        case MouseMode.S =>
          r.height = newHeight

        case MouseMode.SW =>
          r.x -= newWidth - r.width
          r.width = newWidth
          r.height = newHeight

        case MouseMode.SE =>
          r.width = newWidth
          r.height = newHeight

        case MouseMode.E =>
          r.width = newWidth

        case MouseMode.NW =>
          r.x -= newWidth - r.width
          r.y -= newHeight - r.height
          r.width = newWidth
          r.height = newHeight

        case MouseMode.W =>
          r.x -= newWidth - r.width
          r.width = newWidth

        case MouseMode.NE =>
          r.y -= newHeight - r.height
          r.width = newWidth
          r.height = newHeight

        case MouseMode.N =>
          r.y -= newHeight - r.height
          r.height = newHeight

        case _ => throw new IllegalStateException
      }
    }
  }

  def widgetResized(): Unit = {
    super.setBounds(
      if (selected) {
        new Rectangle(getX, getY, widget.getWidth + BorderSize + BorderSize,
                      widget.getHeight + BorderSize + BorderSize)
      } else {
        new Rectangle(getX, getY, widget.getWidth, widget.getHeight)
      }
    )

    revalidateInterfacePanel()
  }

  def handle(e: WidgetForegroundedEvent): Unit = {
    if (e.widget != widget)
      isForeground(false)
  }

  // if we are not selected, return our location if we are selected,
  // return what our location would be if we *weren't* selected... this
  // is needed for the zooming code in InterfacePanel
  def getUnselectedLocation: Point = {
    if (selected) {
      new Point(getX + BorderSize, getY + BorderSize)
    } else {
      getLocation
    }
  }

  def getUnselectedBounds: Rectangle = {
    if (selected) {
      new Rectangle(getX + BorderSize, getY + BorderSize, getWidth - BorderSize - BorderSize,
                    getHeight - BorderSize - BorderSize)
    } else {
      getBounds
    }
  }

  ///

  private def doPopup(e: MouseEvent): Unit = {
    if (interfacePanel != null) {
      val menu = new WrappingPopupMenu

      populateContextMenu(menu, e.getPoint)

      if (menu.getSubElements.size > 0) {
        val point =
          if (getMousePosition() != null) {
            getMousePosition()
          } else {
            e.getPoint
          }

        menu.show(this, point.x, point.y)
      }

      e.consume()
    }
  }

  private def populateContextMenu(menu: PopupMenu, p: Point): Unit = {
    widget.getEditable match {
      case Some(editable) if !interfacePanel.multiSelected =>
        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.edit")) {
          def actionPerformed(e: ActionEvent): Unit = {
            selected(true)
            foreground()
            new EditWidgetEvent(editable).raise(WidgetWrapper.this)
          }
        }))

      case _ =>
    }

    if (selected) {
      menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.deselect")) {
        def actionPerformed(e: ActionEvent): Unit = {
          interfacePanel.setInterfaceMode(InterfaceMode.Select, true)
          selected(false)
          interfacePanel.setForegroundWrapper()
        }
      }))

      if (interfacePanel.multiSelected) {
        menu.addSeparator()

        var added = false

        if (interfacePanel.canAlignLeft) {
          menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.alignLeft")) {
            def actionPerformed(e: ActionEvent): Unit = {
              interfacePanel.alignLeft()
            }
          }))

          added = true
        }

        if (interfacePanel.canAlignCenterHorizontal) {
          menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.alignCenterHorizontal")) {
            def actionPerformed(e: ActionEvent): Unit = {
              interfacePanel.alignCenterHorizontal()
            }
          }))

          added = true
        }

        if (interfacePanel.canAlignRight) {
          menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.alignRight")) {
            def actionPerformed(e: ActionEvent): Unit = {
              interfacePanel.alignRight()
            }
          }))

          added = true
        }

        if (interfacePanel.canAlignTop) {
          menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.alignTop")) {
            def actionPerformed(e: ActionEvent): Unit = {
              interfacePanel.alignTop()
            }
          }))

          added = true
        }

        if (interfacePanel.canAlignCenterVertical) {
          menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.alignCenterVertical")) {
            def actionPerformed(e: ActionEvent): Unit = {
              interfacePanel.alignCenterVertical()
            }
          }))

          added = true
        }

        if (interfacePanel.canAlignBottom) {
          menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.alignBottom")) {
            def actionPerformed(e: ActionEvent): Unit = {
              interfacePanel.alignBottom()
            }
          }))

          added = true
        }

        if (added)
          menu.addSeparator()

        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.distributeHorizontal")) {
          def actionPerformed(e: ActionEvent): Unit = {
            interfacePanel.distributeHorizontal()
          }
        }))

        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.distributeVertical")) {
          def actionPerformed(e: ActionEvent): Unit = {
            interfacePanel.distributeVertical()
          }
        }))

        menu.addSeparator()

        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.stretchLeft")) {
          def actionPerformed(e: ActionEvent): Unit = {
            interfacePanel.stretchLeft()
          }
        }))

        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.stretchRight")) {
          def actionPerformed(e: ActionEvent): Unit = {
            interfacePanel.stretchRight()
          }
        }))

        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.stretchTop")) {
          def actionPerformed(e: ActionEvent): Unit = {
            interfacePanel.stretchTop()
          }
        }))

        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.stretchBottom")) {
          def actionPerformed(e: ActionEvent): Unit = {
            interfacePanel.stretchBottom()
          }
        }))
      }
    } else {
      menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.select")) {
        def actionPerformed(e: ActionEvent): Unit = {
          interfacePanel.setInterfaceMode(InterfaceMode.Select, true)
          selected(true)
          foreground()
        }
      }))
    }

    menu.addSeparator()

    if (interfacePanel.multiSelected) {
      menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.copySelected")) {
        def actionPerformed(e: ActionEvent): Unit = {
          interfacePanel.copySelectedWidgets()
        }
      }))
    } else {
      menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.copy")) {
        def actionPerformed(e: ActionEvent): Unit = {
          interfacePanel.copyWidgets(Seq(WidgetWrapper.this))
        }
      }))
    }

    if (interfacePanel.selectedWrappers.size > 1) {
      menu.addSeparator()

      menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.deleteSelected")) {
        def actionPerformed(e: ActionEvent): Unit = {
          interfacePanel.deleteSelectedWidgets()
        }
      }))
    } else if (widget.deleteable) {
      menu.addSeparator()

      menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.delete")) {
        def actionPerformed(e: ActionEvent): Unit = {
          WidgetActions.removeWidget(interfacePanel, WidgetWrapper.this)
        }
      }))
    }

    if (widget.hasContextMenu) {
      menu.addSeparator()

      widget.populateContextMenu(menu, p)

      if (widget.exportable) {
        menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.export")) {
          def actionPerformed(e: ActionEvent): Unit = {
            new ExportWidgetEvent(widget).raise(WidgetWrapper.this)
          }
        }))
      }

      widget.addExtraMenuItems(menu)
    }
  }

  override def paintComponent(g: Graphics): Unit = {
    val g2d = Utils.initGraphics2D(g)

    if (widget.isVisible) {
      super.paintComponent(g2d)
    } else {
      g2d.setColor(widget.getBackgroundColor)
      g2d.fillRect(widget.getX, widget.getY, widget.getWidth, widget.getHeight)
    }

    if (selected) {
      g2d.setColor(InterfaceColors.widgetHandle())

      // bounding box
      g2d.drawRect(GrabBuffer + HandleSize / 2, GrabBuffer + HandleSize / 2, getWidth - HandleSize - GrabBuffer * 2,
                   getHeight - HandleSize - GrabBuffer * 2)

      // left/right central handles
      g2d.fillRect(GrabBuffer, getHeight / 2 - HandleSize / 2, HandleSize, HandleSize)
      g2d.fillRect(getWidth - HandleSize - GrabBuffer, getHeight / 2 - HandleSize / 2, HandleSize, HandleSize)

      // top/bottom central handles
      g2d.fillRect(getWidth / 2 - HandleSize / 2, GrabBuffer, HandleSize, HandleSize)
      g2d.fillRect(getWidth / 2 - HandleSize / 2, getHeight - HandleSize - GrabBuffer, HandleSize, HandleSize)

      // corner handles
      g2d.fillRect(GrabBuffer, GrabBuffer, HandleSize, HandleSize)
      g2d.fillRect(getWidth - HandleSize - GrabBuffer, GrabBuffer, HandleSize, HandleSize)
      g2d.fillRect(GrabBuffer, getHeight - HandleSize - GrabBuffer, HandleSize, HandleSize)
      g2d.fillRect(getWidth - HandleSize - GrabBuffer, getHeight - HandleSize - GrabBuffer, HandleSize, HandleSize)
    }
  }

  override def syncTheme(): Unit = {
    widget.syncTheme()
  }

  private class ShadowPane extends JPanel {
    setOpaque(false)

    override def paintComponent(g: Graphics): Unit = {
      val g2d = Utils.initGraphics2D(g)

      if (interfacePanel.getInterfaceMode != InterfaceMode.Interact &&
          (interfacePanel.getInterfaceMode != InterfaceMode.Add || placing) &&
          !selected && !highlighted && (widget.isNote || !dragging)) {

        g2d.setColor(InterfaceColors.widgetPreviewCover())

        widget match {
          case _: ViewWidget =>
            g2d.fillRect(widget.getX, widget.getY, widget.getWidth, widget.getHeight)

          case _ =>
            if (widget.isNote)
              g2d.setColor(InterfaceColors.widgetPreviewCoverNote())

            g2d.fillRoundRect(widget.getX, widget.getY, widget.getWidth, widget.getHeight, widget.getDiameter, widget.getDiameter)
        }
      }
    }
  }
}
