// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app.interfacetab

import java.awt.Point
import java.awt.event.{ ActionEvent, FocusEvent, FocusListener, KeyEvent }
import java.awt.image.BufferedImage
import javax.swing.AbstractAction

import org.nlogo.api.{ Exceptions, Version }
import org.nlogo.app.common.{ FileActions, UndoRedoActions },
  FileActions.ExportInterfaceAction
import org.nlogo.awt.Images
import org.nlogo.core.{
  AgentKind, I18N, Button => CoreButton, Chooser => CoreChooser, InputBox => CoreInputBox, Monitor => CoreMonitor,
  Output => CoreOutput, Plot => CorePlot, Slider => CoreSlider, Switch => CoreSwitch, TextBox => CoreTextBox,
  View => CoreView, Widget => CoreWidget }
import org.nlogo.editor.{ Colorizer, EditorArea }
import org.nlogo.log.LogManager
import org.nlogo.swing.{ MenuItem, PopupMenu }
import org.nlogo.window.{ ButtonWidget, ChooserWidget, ClipboardUtils, Editable, Events => WindowEvents, GUIWorkspace,
                          InputBoxWidget, InterfaceGlobalWidget, InterfaceMode, MonitorWidget, PlotWidget,
                          SliderWidget, SwitchWidget, ViewWidget, ViewWidgetInterface, Widget, WidgetInfo,
                          WidgetRegistry },
  WindowEvents.{ CompileAllEvent, LoadBeginEvent, LoadWidgetsEvent, RemoveConstraintEvent, WidgetRemovedEvent }
import org.nlogo.workspace.Evaluator

class InterfacePanel(val viewWidget: ViewWidgetInterface, workspace: GUIWorkspace, colorizer: Colorizer)
  extends WidgetPanel(workspace)
  with FocusListener
  with LoadWidgetsEvent.Handler
  with UndoRedoActions {

  // in 3d don't add the view widget since it's always
  // disabled there's no reason for it to take space 7/5/07
  if (!Version.is3D)
    addWidget(viewWidget.asInstanceOf[Widget], 0, 0, false, false)

  viewWidget.asInstanceOf[Widget].deleteable = false
  addFocusListener(this)

  ///

  override def focusGained(e: FocusEvent): Unit = {
    enableButtonKeys(true)
  }

  override def focusLost(e: FocusEvent): Unit = {
    enableButtonKeys(false)
  }

  ///

  override protected def doPopup(point: Point): Unit = {
    if (interfaceMode == InterfaceMode.Interact)
      interceptPane.disableIntercept()

    val menu = new PopupMenu

    Seq(WidgetInfo.button,
      WidgetInfo.slider,
      WidgetInfo.switch,
      WidgetInfo.chooser,
      WidgetInfo.input,
      WidgetInfo.monitor,
      WidgetInfo.plot)
    .map(i => i.displayName -> i.widgetThunk)
    .foreach {
        case (displayName, widgetThunk) =>
          menu.add(new WidgetCreationMenuItemIP(displayName, widgetThunk()))
    }

    // add all the widgets
    val outputItem = new WidgetCreationMenuItemIP(I18N.gui.get("tabs.run.widgets.output"), CoreOutput(0, 0, 0, 0, 11))
    if (getOutputWidget != null) {
      outputItem.setEnabled(false)
    }
    menu.add(outputItem)

    menu.add(new WidgetCreationMenuItemIP(I18N.gui.get("tabs.run.widgets.note"), CoreTextBox(None, fontSize = 11)))

    menu.addSeparator()

    menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widget.copySelected")) {
      def actionPerformed(e: ActionEvent): Unit = {
        copySelectedWidgets()
      }
    })).setEnabled(selectedWrappers.nonEmpty)

    menu.add(new MenuItem(new AbstractAction(I18N.gui.get("tabs.run.widgets.paste")) {
      def actionPerformed(e: ActionEvent): Unit = {
        pasteWidgets()
      }
    })).setEnabled(ClipboardUtils.hasWidgets)

    // add extra stuff
    menu.addSeparator()
    menu.add(new MenuItem(new ExportInterfaceAction(workspace, this)))

    menu.show(this, point.x, point.y)
  }

  class WidgetCreationMenuItemIP(val displayName: String, val coreWidget: CoreWidget)
    extends MenuItem(new AbstractAction(displayName) {
      def actionPerformed(e: ActionEvent): Unit = {
        unselectWidgets()
        createShadowWidget(coreWidget)
      }
    })

  // This is used both when loading a model and when the user is making
  // new widgets in the UI.  For most widget types, the same type string
  // is used in both places. - ST 3/17/04
  override def makeWidget(coreWidget: CoreWidget): Widget = {
    val fromRegistry = WidgetRegistry(coreWidget.getClass.getSimpleName)
    if (fromRegistry != null)
      fromRegistry
    else coreWidget match {
      case c: CoreChooser  => new ChooserWidget(workspace, colorizer, workspace.getExtensionManager)
      case b: CoreButton   => new ButtonWidget(workspace.world.mainRNG, colorizer)
      case p: CorePlot     => PlotWidget(workspace.plotManager, colorizer)
      case m: CoreMonitor  => new MonitorWidget(workspace.world.auxRNG, workspace, colorizer)
      case s: CoreSlider =>
        new SliderWidget(workspace.world.auxRNG, workspace, colorizer, workspace.getExtensionManager) {
          override def sourceOffset: Int =
            Evaluator.sourceOffset(AgentKind.Observer, false)
        }
      case s: CoreSwitch => new SwitchWidget(workspace, workspace.getExtensionManager)
      case i: CoreInputBox =>
        val textArea       = new EditorArea(textEditorConfiguration)
        val dialogTextArea = new EditorArea(dialogEditorConfiguration)

        new InputBoxWidget(textArea, dialogTextArea, workspace, workspace.getExtensionManager, this)
      case v: CoreView => new ViewWidget(workspace)
      case _ =>
        throw new IllegalStateException("unknown widget type: " + coreWidget.getClass.getName)
    }
  }

  override private[app] def deleteWidgets(hitList: Seq[WidgetWrapper]): Unit = {
    var needsRecompile: Boolean = false
    for (wrapper <- hitList) {
      removeWidget(wrapper)
      wrapper.widget match {
        case _: InterfaceGlobalWidget => needsRecompile = true
        case _ =>
      }
    }
    setForegroundWrapper()
    revalidate()
    repaint() // you wouldn't think this'd be necessary, but without it
    // the widget didn't visually disappear - ST 6/23/03
    if (needsRecompile) {
      new CompileAllEvent().raise(this)
    }
  }

  override protected def removeWidget(wrapper: WidgetWrapper): Unit = {
    remove(wrapper)

    // if the compile that is associated with this removal (assuming there is one) fails
    // the observer variables and constraints might not get reallocated in which case
    // if we try to add a different widget with the same name we get a constraint violation
    // from the old constraint. yuck.  ev 11/27/07
    new RemoveConstraintEvent(wrapper.widget.displayName).raise(this)

    LogManager.widgetRemoved(true, wrapper.widget.classDisplayName, wrapper.widget.displayName)
  }

  /// loading and saving

  override def loadWidget(coreWidget: CoreWidget): Widget =
    loadWidget(coreWidget, 0, 0)

  // TODO: consider cleaning up this x and y business
  // it was added for copying/pasting widgets.
  // the regular loadWidget just uses the x and y from the string array
  // it passes in x=0, y=0 and we do a check. ugly, but works for now.
  // paste uses the x and y from the right click location.
  private def loadWidget(coreWidget: CoreWidget, _x: Int, _y: Int): Widget = {
    val x = if (_x == 0) coreWidget.x else _x
    val y = if (_y == 0) coreWidget.y  else _y
    coreWidget match {
      case view: CoreView =>
        // the graphics widget (and the command center) are special cases because
        // they are not recreated at load time, but reused
        viewWidget.load(view)
        // in 3D we don't add the viewWidget to the interface panel
        // so don't worry about all the sizing junk ev 7/5/07
        val parent = viewWidget.getParent
        if (parent != null) {
          parent.setSize(viewWidget.getSize)
          enforceMinimumAndMaximumWidgetSizes(viewWidget)
          parent.setLocation(x, y)
          zoomer.zoomWidgetLocation(
            getWrapper(viewWidget),
                  true, true, 1.0, zoomer.zoomFactor)
          zoomer.zoomWidgetSize(
            getWrapper(viewWidget),
                  true, true, 1.0, zoomer.zoomFactor)
          zoomer.scaleComponentFont(
            viewWidget.asInstanceOf[ViewWidget].view,
                 zoomFactor, 1.0, false)
        }
        viewWidget
      case _ =>
        makeAndLoadWidget(coreWidget, x, y)
    }
  }

  override def getWidgetsForSaving: Seq[CoreWidget] =
    // automatically add the view widget since it isn't in
    // the components list in 3D - ev 7/5/07
    (viewWidget.model +: getComponents.reverse.collect {
      case wrapper: WidgetWrapper => wrapper.widget.model
    }).distinct.toIndexedSeq

  override private[app] def contains(w: Editable): Boolean =
    if (viewWidget.getEditable.contains(w))
      true
    else
      super.contains(w)

  override def handle(e: WidgetRemovedEvent): Unit = {
    // We use `raiseLater` to ensure that the WidgetRemovedEvent
    // propagates to the Compiler.
    if (e.widget.getWidgetContainer.contains(this) && !unloading)
      new CompileAllEvent().raiseLater(this)
  }

  def interfaceImage: BufferedImage =
    Images.paintToImage(this)

  def handle(e: LoadWidgetsEvent): Unit = {
    loadWidgets(e.widgets, e.widgetSizesOption)
  }

  private var unloading = false

  override def handle(e: LoadBeginEvent): Unit = {
    unloading = true
    super.handle(e)
    unloading = false
  }

  override def removeAllWidgets(): Unit = {
    try {
      setVisible(false)
      for (component <- getComponents) {
        component match {
          case w: WidgetWrapper if w.widget != viewWidget =>
            removeWidget(w)
          case _ =>
        }
      }
    } catch {
      case ex: RuntimeException => Exceptions.handle(ex)
    } finally {
      setVisible(false)
    }
  }

  /// buttons

  private def findActionButton(key: Char): ButtonWidget = {
    getComponents.collect {
      case w: WidgetWrapper => w.widget
    }.collect {
      case b: ButtonWidget if b.actionKey.toUpper == key.toUpper => b
    }.headOption.orNull
  }

  private def enableButtonKeys(enabled: Boolean): Unit =
    getComponents.collect {
      case w: WidgetWrapper => w.widget
    }.foreach {
      case b: ButtonWidget => b.keyEnabled(enabled)
      case _ =>
    }

  override def keyTyped(e: KeyEvent): Unit = {
    if (e.getKeyChar() != KeyEvent.CHAR_UNDEFINED &&
      !e.isActionKey &&
    (e.getModifiersEx & getToolkit.getMenuShortcutKeyMaskEx) == 0) {
      Option(findActionButton(e.getKeyChar)).foreach { button =>
        button.keyTriggered()
      }
    }
  }

  override def canAddWidget(widget: String): Boolean = {
    return (widget != "Output" || getOutputWidget == null)
  }
}
