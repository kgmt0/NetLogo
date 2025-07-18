// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo
package org.nlogo.headless

import java.util.Locale

import org.nlogo.workspace
import workspace.WorldLoader
import org.nlogo.plot.PlotLoader
import org.nlogo.agent.{BooleanConstraint, ChooserConstraint, CompilationManagement, InputBoxConstraint, NumericConstraint}
import org.nlogo.api.{ LabProtocol, PreviewCommands, SourceOwner, ValueConstraint, Version }
import org.nlogo.core.{ Button, CompilerException, ConstraintSpecification,
  LogoList, Model, Monitor, Program, Shape },
  ConstraintSpecification._,
  Shape.{ LinkShape => CoreLinkShape, VectorShape => CoreVectorShape }
import org.nlogo.api.PlotCompilationErrorAction

import org.nlogo.shape.{ShapeConverter, LinkShape, VectorShape}

// this class is an abomination
// everything works off of side effects, asking the workspace to update something
// but not only that, some of the internals of this class work off side effects as well
// when they don't have to. - JC 10/27/09
class HeadlessModelOpener(ws: HeadlessWorkspace) {
  def stripLines(st: String): String =
    st.flatMap{
      case '\n' => "\\n"
      case '\\' => "\\\\"
      case '\"' => "\\\""
      case c => c.toString
    }

  def openFromModel(model: Model, shouldAutoInstallLibs: Boolean = false): Unit = {
    require(!ws.modelOpened, "HeadlessWorkspace can only open one model")
    ws.setOpenModel(model)

    // get out if unknown version
    val netLogoVersion = model.version
    if (!Version.compatibleVersion(netLogoVersion))
      throw new IllegalStateException("unknown NetLogo version: " + netLogoVersion)

    WorldLoader.load(model.view, ws)

    for(plot <- model.plots)
      PlotLoader.loadPlot(plot, ws.plotManager.newPlot(""))

    // load system dynamics model (if present)
    ws.aggregateManager.load(model, ws)

    // read procedures, compile them.
    val results = {
      val additionalSources: Seq[SourceOwner] = if (ws.aggregateManager.isLoaded) Seq(ws.aggregateManager) else Seq()
      val code = model.code
      ws.compiler.compileProgram(
        code, additionalSources, Program.empty().copy(interfaceGlobals = model.interfaceGlobals),
        ws.getExtensionManager, ws.getLibraryManager, ws.compilationEnvironment,
        shouldAutoInstallLibs)
    }
    ws.procedures = results.proceduresMap
    ws.clearRunCache()

    // Read preview commands. If the model doesn't specify preview commands, the default ones will be used.
    model.optionalSectionValue[PreviewCommands]("org.nlogo.modelsection.previewcommands").foreach(ws.setPreviewCommands(_))

    // parse turtle and link shapes, updating the workspace.
    parseShapes(model.turtleShapes, model.linkShapes)

    ws.getPrimaryWorkspace.getExperimentManager.setGUIExperiments(
      model.optionalSectionValue[Seq[LabProtocol]]("org.nlogo.modelsection.behaviorspace").getOrElse(Seq()))

    ws.init()
    ws.world.asInstanceOf[CompilationManagement].program = results.program

    // test code is mixed with actual code here, which is a bit funny.
    if (ws.compilerTestingMode)
      testCompileWidgets(results.program,
        model.widgets.collect { case b: Button => b },
        model.widgets.collect { case m: Monitor => m })
    else
      finish(model.constraints, results.program, model.interfaceGlobalCommands.mkString("\n"), ws.getPlotCompilationErrorAction())
    }


  private def parseShapes(turtleShapes: Seq[CoreVectorShape], linkShapes: Seq[CoreLinkShape]): Unit = {
    ws.world.turtleShapes.replaceShapes(
      turtleShapes.map(ShapeConverter.baseVectorShapeToVectorShape))
    if (turtleShapes.isEmpty)
      ws.world.turtleShapes.add(VectorShape.getDefaultShape)

    // A new model is being loaded, so get rid of all previous shapes
    ws.world.linkShapes.replaceShapes(
      linkShapes.map(ShapeConverter.baseLinkShapeToLinkShape))
    if (linkShapes.isEmpty)
      ws.world.linkShapes.add(LinkShape.getDefaultLinkShape)
  }

  /**
  *  @param plotCompilationErrorAction  action to take if a plot compilation error occurs
  */
  private def finish(constraints: Map[String, ConstraintSpecification], program: Program,
                     interfaceGlobalCommands: String, plotCompilationErrorAction: PlotCompilationErrorAction): Unit = {
    ws.world.realloc()

    val errors = ws.plotManager.compileAllPlots()
    if (errors.nonEmpty) {
      plotCompilationErrorAction match {
        case PlotCompilationErrorAction.Throw => throw errors(0)
        case PlotCompilationErrorAction.Output => errors.foreach { println }
        case PlotCompilationErrorAction.Ignore =>
      }
    }

    for ((vname, spec) <- constraints) {
      val con: ValueConstraint = spec match {
        case NumericConstraintSpecification(default) => new NumericConstraint(default)
        case ChoiceConstraintSpecification(vals, defaultIndex) => new ChooserConstraint(
          LogoList.fromIterator(vals.iterator),
          defaultIndex
        )
        case BooleanConstraintSpecification(default) => new BooleanConstraint(default)
        case StringInputConstraintSpecification(typeName, default) => new InputBoxConstraint(typeName, default)
        case NumericInputConstraintSpecification(typeName, default) => new InputBoxConstraint(typeName, default)
        case _ => throw new IllegalStateException
      }
      ws.world.observer.setConstraint(ws.world.observerOwnsIndexOf(vname.toUpperCase(Locale.ENGLISH)), con)
    }

    ws.command(interfaceGlobalCommands)
  }

  private def testCompileWidgets(program: Program, buttons: Seq[Button], monitors: Seq[Monitor]): Unit = {
    val errors = ws.plotManager.compileAllPlots()
    if(errors.nonEmpty) throw errors(0)

    for {
      button <- buttons
      source <- button.source
    }
    try ws.compileCommands(source, button.buttonKind)
    catch {
      case ex: CompilerException =>
        println("compiling: \"" + button + "\"")
        throw ex
    }

    for {
      monitor <- monitors
      source <- monitor.source
    }
    try ws.compileReporter(source)
    catch {
      case ex: CompilerException =>
        println("compiling: \"" + monitor + "\"")
        throw ex
    }
  }

}
