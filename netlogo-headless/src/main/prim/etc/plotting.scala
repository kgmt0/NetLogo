// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.{ core, api, nvm },
  core.{ PlotPenInterface => PlotPenModes, Syntax },
  api.{ PlotAction, PlotInterface, PlotManagerInterface, PlotPenInterface },
  nvm.{ Command, Context, RuntimePrimitiveException, Instruction, Reporter }

//
// base classes
//

trait PlotHelpers extends Instruction {
  def plotManager =
    workspace.realPlotManager.asInstanceOf[PlotManagerInterface]
  def currentPlotState(context: Context) =
    currentPlot(context).state
  def currentPlot(context: Context): PlotInterface =
    plotManager.currentPlot.getOrElse(
      throw new RuntimePrimitiveException(
        context, this,
          core.I18N.errors.get("org.nlogo.plot.noPlotSelected")))
  def currentPen(context: Context): PlotPenInterface = {
    val plot = currentPlot(context)
    plot.currentPen.getOrElse(
      throw new RuntimePrimitiveException(
        context, this, "Plot '" + plot.name + "' has no pens!"))
  }
}

abstract class PlotCommand(args: Int*)
extends Command with PlotHelpers {
}

abstract class PlotActionCommand(args: Int*)
extends PlotCommand(args*) {
  override def perform(context: Context): Unit = {
    plotManager.publish(action(context))
    context.ip = next
  }
  def action(context: Context): PlotAction
}

abstract class PlotReporter(returnType: Int, args: Int*)
extends Reporter with PlotHelpers {
}

//
// commands requiring only the plot manager (it's ok if there are no plots)
//

class _clearallplots extends PlotCommand() {
  override def perform(context: Context): Unit = {
    for (name <- plotManager.getPlotNames)
      plotManager.publish(PlotAction.ClearPlot(name))
    context.ip = next
  }
}

class _setupplots extends PlotCommand() {
  override def callsOtherCode = true
  override def perform(context: Context): Unit = {
    workspace.setupPlots(context)
    context.ip = next
  }
}
class _updateplots extends PlotCommand() {
  override def callsOtherCode = true
  override def perform(context: Context): Unit = {
    workspace.updatePlots(context)
    context.ip = next
  }
}
class _setcurrentplot extends PlotCommand(Syntax.StringType) {
  override def perform(context: Context): Unit = {
    val name = argEvalString(context, 0)
    if (!plotManager.hasPlot(name))
      throw new RuntimePrimitiveException(context, this,
        "no such plot: \"" + name + "\"")
    plotManager.setCurrentPlot(name)
    context.ip = next
  }
}

//
// commands requiring that there be a current plot.
//

class _clearplot extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.ClearPlot(currentPlot(context).name)
}

class _autoplotoff extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.AutoPlot(currentPlot(context).name, on = false)
}
class _autoploton extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.AutoPlot(currentPlot(context).name, on = true)
}

class _autoplotxoff extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.AutoPlotX(currentPlot(context).name, on = false)
}

class _autoplotxon extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.AutoPlotX(currentPlot(context).name, on = true)
}

class _autoplotyoff extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.AutoPlotY(currentPlot(context).name, on = false)
}

class _autoplotyon extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.AutoPlotY(currentPlot(context).name, on = true)
}

class SetPlotRangeCommand(isX: Boolean) extends PlotActionCommand(Syntax.NumberType, Syntax.NumberType) {
  override def action(context: Context) = {
    val min = argEvalDoubleValue(context, 0)
    val max = argEvalDoubleValue(context, 1)
    if (min >= max)
      throw new RuntimePrimitiveException(
        context, this,
        "the minimum must be less than the maximum, but " + min +
        " is greater than or equal to " + max)
    PlotAction.SetRange(
      plotName = currentPlot(context).name,
      isX = isX, min = min, max = max)
  }
}
class _setplotxrange extends SetPlotRangeCommand(isX = true)
class _setplotyrange extends SetPlotRangeCommand(isX = false)

class _createtemporaryplotpen extends PlotActionCommand(Syntax.StringType) {
  override def action(context: Context) =
    PlotAction.CreateTemporaryPen(
      currentPlot(context).name, argEvalString(context, 0))
}

class _exportplot extends PlotCommand(Syntax.StringType, Syntax.StringType) {
  override def perform(context: Context): Unit = {
    val name = argEvalString(context, 0)
    val path = argEvalString(context, 1)
    if (plotManager.hasPlot(name))
      throw new RuntimePrimitiveException(
        context, this, "no such plot: \"" + name + "\"")
    // Workspace.waitFor() switches to the event thread if we're running with a GUI - ST 12/17/04
    workspace.waitFor(new api.CommandRunnable {
      def run(): Unit = {
        try workspace.exportPlot(name, workspace.fileManager.attachPrefix(path))
        catch {
          case ex: java.io.IOException =>
            throw new RuntimePrimitiveException(
              context, _exportplot.this, token.text + ": " + ex.getMessage)
        }
      }
    })
    context.ip = next
  }
}

// this also requires only the PlotManager, but it seems better to put it here next to exportplot.
class _exportplots extends PlotCommand(Syntax.StringType) {
  override def perform(context: Context): Unit = {
    val path = argEvalString(context, 0)
    if (plotManager.getPlotNames.isEmpty)
      throw new RuntimePrimitiveException(context, this, "there are no plots to export")
    // Workspace.waitFor() switches to the event thread if we're running with a GUI - ST 12/17/04
    workspace.waitFor(new api.CommandRunnable {
      def run(): Unit = {
        try workspace.exportAllPlots(workspace.fileManager.attachPrefix(path))
        catch {
          case ex: java.io.IOException =>
            throw new RuntimePrimitiveException(context, _exportplots.this,
              token.text + ": " + ex.getMessage)
        }
      }
    })
    context.ip = next
  }
}

//
// reporters
//

class _autoplot extends PlotReporter(Syntax.BooleanType) {
  override def report(context: Context): java.lang.Boolean =
    Boolean.box(currentPlotState(context).autoPlotX && currentPlotState(context).autoPlotY)
}
class _autoplotx extends PlotReporter(Syntax.BooleanType) {
  override def report(context: Context): java.lang.Boolean =
    Boolean.box(currentPlotState(context).autoPlotX)
}
class _autoploty extends PlotReporter(Syntax.BooleanType) {
  override def report(context: Context): java.lang.Boolean =
    Boolean.box(currentPlotState(context).autoPlotY)
}
class _plotname extends PlotReporter(Syntax.StringType) {
  override def report(context: Context): String =
    currentPlot(context).name
}
class _plotxmin extends PlotReporter(Syntax.NumberType) {
  override def report(context: Context): java.lang.Double =
    Double.box(currentPlotState(context).xMin)
}
class _plotxmax extends PlotReporter(Syntax.NumberType) {
  override def report(context: Context): java.lang.Double =
    Double.box(currentPlotState(context).xMax)
}
class _plotymin extends PlotReporter(Syntax.NumberType) {
  override def report(context: Context): java.lang.Double =
    Double.box(currentPlotState(context).yMin)
}
class _plotymax extends PlotReporter(Syntax.NumberType) {
  override def report(context: Context): java.lang.Double =
    Double.box(currentPlotState(context).yMax)
}
class _plotpenexists extends PlotReporter(Syntax.BooleanType, Syntax.StringType) {
  override def report(context: Context): java.lang.Boolean =
    Boolean.box(currentPlot(context).getPen(argEvalString(context, 0)).isDefined)
}

//
// plot pen prims
//

class _plot extends PlotActionCommand(Syntax.NumberType) {
  override def action(context: Context) =
    PlotAction.PlotY(
      currentPlot(context).name,
      currentPen(context).name,
      argEvalDoubleValue(context, 0))
}

class _plotxy extends PlotActionCommand(Syntax.NumberType, Syntax.NumberType) {
  override def action(context: Context) =
    PlotAction.PlotXY(
      currentPlot(context).name,
      currentPen(context).name,
      argEvalDoubleValue(context, 0),
      argEvalDoubleValue(context, 1))
}

class _histogram extends PlotCommand(Syntax.ListType) {
  override def perform(context: Context): Unit = {
    val list = argEvalList(context, 0)
    val pen = currentPen(context)
    if (pen.state.interval <= 0)
      throw new RuntimePrimitiveException(context, this,
        "You cannot histogram with a plot-pen-interval of " + api.Dump.number(pen.state.interval) + ".")
    val values = list.scalaIterator.collect {
      case d: java.lang.Double =>
        d.doubleValue
    }.toSeq
    currentPlot(context)
      .histogramActions(pen, values)
      .foreach(plotManager.publish)
    context.ip = next
  }
}

class _sethistogramnumbars extends PlotActionCommand(Syntax.NumberType) {
  override def action(context: Context) = {
    val numBars = argEvalIntValue(context, 0)
    if (numBars < 1)
      throw new RuntimePrimitiveException(context, this,
        "You cannot make a histogram with " + numBars + " bars.")
    val plot = currentPlot(context)
    val pen = currentPen(context)
    val newInterval = (plot.state.xMax - plot.state.xMin) / numBars
    PlotAction.SetPenInterval(
      plot.name, pen.name, newInterval)
  }
}
class _setplotpeninterval extends PlotActionCommand(Syntax.NumberType) {
  override def action(context: Context) =
    PlotAction.SetPenInterval(
      currentPlot(context).name,
      currentPen(context).name,
      argEvalDoubleValue(context, 0))
}

class _plotpendown extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.PenDown(
      currentPlot(context).name,
      currentPen(context).name,
      down = true)
}
class _plotpenup extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.PenDown(
      currentPlot(context).name,
      currentPen(context).name,
      down = false)
}
class _plotpenshow extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.HidePen(
      currentPlot(context).name,
      currentPen(context).name,
      hidden = false)
}
class _plotpenhide extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.HidePen(
      currentPlot(context).name,
      currentPen(context).name,
      hidden = true)
}
class _plotpenreset extends PlotActionCommand() {
  override def action(context: Context) =
    PlotAction.HardResetPen(
      currentPlot(context).name,
      currentPen(context).name)
}

class _setplotpenmode extends PlotActionCommand(Syntax.NumberType) {
  override def action(context: Context) = {
    val mode = argEvalIntValue(context, 0)
    if (mode < PlotPenModes.MinMode || mode > PlotPenModes.MaxMode) {
      throw new RuntimePrimitiveException(context, this,
        s"$mode is not a valid plot pen mode (valid modes are 0, 1, and 2)")
    }
    val plotName = currentPlot(context).name
    val penName = currentPen(context).name
    PlotAction.SetPenMode(plotName, penName, mode)
  }
}

class _setplotpencolor extends PlotActionCommand(Syntax.NumberType) {
  override def action(context: Context) = {
    val color =
      api.Color.getARGBbyPremodulatedColorNumber(
        api.Color.modulateDouble(argEvalDoubleValue(context, 0)))
    val plotName = currentPlot(context).name
    val penName = currentPen(context).name
    PlotAction.SetPenColor(plotName, penName, color)
  }
}

class _setcurrentplotpen extends PlotCommand(Syntax.StringType) {
  override def perform(context: Context): Unit = {
    val name = argEvalString(context, 0)
    if (!currentPlot(context).getPen(name).isDefined)
      throw new RuntimePrimitiveException(context, this,
        "There is no pen named \"" + name + "\" in the current plot")
    currentPlot(context).currentPenByName = name
    context.ip = next
  }
}
