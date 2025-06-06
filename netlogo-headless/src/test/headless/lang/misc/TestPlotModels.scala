// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.headless
package lang
package misc

import org.nlogo.plot.PlotPen
import org.nlogo.core._
import org.nlogo.api.PlotCompilationErrorAction

class TestPlotModels extends FixtureSuite {

  // convenience
  def containsPoint(pen: PlotPen, x: Double, y: Double) =
    pen.points.exists(p => p.x == x && p.y == y)

  def onlyPlot(implicit fixture: Fixture) =
    fixture.workspace.plotManager.plots match {
      case Seq(plot) => plot
      case p => throw new IllegalStateException
    }
  def onlyPen(implicit fixture: Fixture) =
    onlyPlot.pens match {
      case Seq(pen) => pen
      case p => throw new IllegalStateException
    }

  val modelCode =
    """|breed [dogs dog]
       |to setup
       |  clear-all
       |  reset-ticks
       |end
       |to go
       |  create-dogs 1
       |  tick
       |end""".stripMargin
  val theModel =
    Model(code = modelCode, widgets = List(
      View(), Plot(display = Some(""), pens = List(Pen(display = "", updateCode = "plot count dogs * 2")))))

  test("plot on tick") { implicit fixture =>
    import fixture._
    openModel(theModel)
    assertResult(1)(workspace.plotManager.plots.size)
    assertResult(1)(onlyPlot.pens.size)
    testCommand("setup")
    assertResult(1)(onlyPen.points.size)
    assert(containsPoint(onlyPen, 0.0, 0.0))
    testCommand("go")
    testReporter("count dogs", "1")
    testReporter("ticks", "1")
    assert(containsPoint(onlyPen, 1.0, 2.0))

    testCommand("go")
    testReporter("count dogs", "2")
    testReporter("ticks", "2")
    assert(containsPoint(onlyPen, 2.0, 4.0))

    testCommand("tick")
    assert(containsPoint(onlyPen, 3.0, 4.0))
  }

  test("several ticks") { implicit fixture =>
    import fixture._
    openModel(theModel)
    testCommand("setup")
    testCommand("tick")
    assert(containsPoint(onlyPen, 0.0, 0.0))
    assert(containsPoint(onlyPen, 1.0, 0.0))

    testCommand("tick")
    assert(containsPoint(onlyPen, 2.0, 0.0))

    testCommand("create-dogs 1")
    testCommand("tick")
    assert(containsPoint(onlyPen, 3.0, 2.0))
  }

  test("update-plots") { implicit fixture =>
    import fixture._
    openModel(theModel)
    testCommand("setup")
    assertResult(1)(onlyPen.points.size)
    assert(containsPoint(onlyPen, 0.0, 0.0))
    testCommand("update-plots")
    assertResult(2)(onlyPen.points.size)
    assert(containsPoint(onlyPen, 1.0, 0.0))

    testCommand("update-plots")
    assertResult(3)(onlyPen.points.size)
    assert(containsPoint(onlyPen, 2.0, 0.0))

    testCommand("create-dogs 1")
    testReporter("count dogs", "1")
    testCommand("update-plots")
    assertResult(4)(onlyPen.points.size)
    assert(containsPoint(onlyPen, 3.0, 2.0))

    testCommand("update-plots")
    assertResult(5)(onlyPen.points.size)
    assert(containsPoint(onlyPen, 4.0, 2.0))
  }

  test("setup-plots") { implicit fixture =>
    import fixture._
    openModel(
      Model(code = modelCode, widgets = List(
        View(),
        Plot(display = Some(""), setupCode = "create-dogs 5",
          pens = List(Pen(display = "", updateCode = "plot count dogs * 2"))))))
    testCommand("setup-plots")
    testReporter("count dogs", "5")
  }

  test("plot with setup code and pen with setup code") { implicit fixture =>
    import fixture._
    openModel(Model(code = modelCode, widgets = List(
      View(),
      Plot(display = Some(""), setupCode = "create-dogs 5",
           pens = List(Pen(display = "", setupCode = "create-dogs 3"))))))
    testReporter("count dogs", "0")
    testCommand("setup-plots")
    testReporter("count dogs", "8")
  }

  test("pen with no update code should not get plotted on tick") { implicit fixture =>
    import fixture._
    openModel(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), pens = List(Pen(display = "", updateCode = ""))))))
    testCommand("reset-ticks")
    assert(onlyPen.points.size === 0)
    testCommand("tick")
    assert(onlyPen.points.size === 0)
  }

  test("plot update code should run on tick") { implicit fixture =>
    import fixture._
    openModel(
      Model(code = modelCode, widgets = List(View(),
                                             Plot(display = Some(""), updateCode = "plot count turtles",
                                                  pens = List(Pen(display = ""))))))
    testCommand("reset-ticks clear-all-plots")
    assert(onlyPen.points.size === 0)

    testCommand("tick")
    assert(containsPoint(onlyPen, 0.0, 0.0))
    assert(onlyPen.points.size === 1)
  }

  test("two plots with setup code") { implicit fixture =>
    import fixture._
    openModel(
      Model(code = modelCode, widgets = List(
        View(),
        Plot(display = Some(""), setupCode = "create-dogs 5", pens = List(Pen(display = "", updateCode = "plot count dogs * 2"))),
        Plot(display = Some(""), setupCode = "create-dogs 2", pens = List(Pen(display = "", updateCode = "plot count dogs * 2")))
        )))
    testReporter("count dogs", "0")
    testCommand("setup-plots")
    testReporter("count dogs", "7")
  }

  test("stop in plot update code") { implicit fixture =>
    import fixture._
    openModel(
      Model(code = modelCode, widgets = List(
        View(),
        Plot(display = Some(""), updateCode = "create-dogs 7 stop", pens = List(Pen(display = "", updateCode = "create-dogs 8"))))))
    testReporter("count dogs", "0")
    testCommand("update-plots")
    testReporter("count dogs", "7")
  }

  val modelCode2 = "breed [dogs dog] to go tick create-dogs 4 end"
  test("stop in plot update code doesnt kill outer procedure") { implicit fixture =>
    import fixture._
    openModel(
      Model(modelCode2,
        widgets = List(
          View(),
          Plot(display = Some(""), updateCode = "create-dogs 1 stop",
            pens = List(Pen(display = "", updateCode = "create-dogs 42"))))))
    testCommand("ca")
    testReporter("count dogs", "0")
    testCommand("reset-ticks")
    // reset ticks calls the plot update code, which creates 1 dog.
    // it then uses stop, so the pen code doesnt create 42 dogs.
    // so only one dog gets created
    testReporter("count dogs", "1")
    testCommand("go")
    // go runs the plot code again, creating 1 dog. the pen code doesnt run.
    // the outer procedure "go" is not stopped, and it creates 4 more dogs.
    testReporter("count dogs", "6")
  }

  // same exact test as the previous test, just call update-plots directly instead of tick.
  val modelCode3 = "breed [dogs dog] to go update-plots create-dogs 4 end"
  test("stop in plot update code doesnt kill outer procedure (2)") { implicit fixture =>
    import fixture._
    openModel(
      Model(modelCode3, widgets = List(View(), Plot(display = Some(""), updateCode = "create-dogs 1 stop",
                                  pens = List(Pen(display = "", updateCode = "create-dogs 42"))))))
    testCommand("ca")
    testReporter("count dogs", "0")
    testCommand("reset-ticks")
    // reset ticks calls the plot update code, which creates 1 dog.
    // it then uses stop, so the pen code doesnt create 42 dogs.
    // so only one dog gets created
    testReporter("count dogs", "1")
    testCommand("go")
    // go runs the plot code again, creating 1 dog. the pen code doesnt run.
    // the outer procedure "go" is not stopped, and it creates 4 more dogs.
    testReporter("count dogs", "6")
  }

  test("inner stop doesnt prevent pens from running") { implicit fixture =>
    import fixture._
    openModel(
      Model(modelCode, widgets = List(
        View(),
        Plot(display = Some(""), updateCode = "ask turtles [stop]",
             pens = List(Pen(display = "", updateCode = "create-dogs 8"))))))
    testReporter("count dogs", "0")
    testCommand("update-plots")
    testReporter("count dogs", "8")
  }

  test("stop in pen doesnt prevent other pens from running") { implicit fixture =>
    import fixture._
    openModel(
      Model(modelCode, widgets = List(
        View(),
        Plot(display = Some(""), pens = List(Pen(display = "", updateCode = "create-dogs 8 stop"),
                                            Pen(display = "", updateCode = "create-dogs 8 stop"))))))
    testReporter("count dogs", "0")
    testCommand("update-plots")
    testReporter("count dogs", "16")
  }

  /**
   * from the user manual:
   *   random-seed 10
   *   with-local-randomness [ print n-values 10 [random 10] ]
   *   ;; prints [8 9 8 4 2 4 5 4 7 9]
   *   print n-values 10 [random 10]
   *   ;; prints [8 9 8 4 2 4 5 4 7 9]
   */
  val modelCode4 = "globals [x]"
  test("plot code uses aux rng") { implicit fixture =>
    import fixture._
    openModel(
      Model(modelCode4, widgets = List(
        View(),
        Plot(display = Some(""), updateCode = "set x n-values 10 [random 10]",
             pens = List(Pen(display = "", updateCode = "set x n-values 10 [random 10]"))))))
    testCommand("reset-ticks")
    testCommand("random-seed 10")
    testReporter("n-values 10 [random 10]", "[8 9 8 2 0 4 3 4 7 9]")
    testCommand("random-seed 10")
    testCommand("tick") // runs plot code, which uses rng in this test case
    testReporter("n-values 10 [random 10]", "[8 9 8 2 0 4 3 4 7 9]")
  }

  test("legend is correctly off") { implicit fixture =>
    import fixture._
    openModel(Model("", widgets = List( View(), Plot(display = Some(""), updateCode = "", pens = List()))))
    assertResult(false)(workspace.plotManager.currentPlot.get.legendIsOpen)
  }

  test("legend is correctly on") { implicit fixture =>
    import fixture._
    openModel(Model("", widgets = List( View(), Plot(display = Some(""), updateCode = "", pens = List(), legendOn = true))))
    assertResult(true)(workspace.plotManager.currentPlot.get.legendIsOpen)
  }

  def testCompileError(model: Model)(f: Throwable => Unit)(implicit fixture: Fixture) = {
    val ex = intercept[Throwable] {
      fixture.workspace.openModel(model)
    }
    f(ex)
  }

  test("Plot With Bad Update Code Should Throw Exception on Load (headless only)") { implicit fixture =>
    testCompileError(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), updateCode="weijefwef")))) { ex =>
      assert("Nothing named WEIJEFWEF has been defined." === ex.getMessage)
    }}

  test("Plot With Bad Setup Code Should Throw Exception on Load (headless only)") { implicit fixture =>
    testCompileError(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), setupCode="weijefwef")))){ ex =>
      assert("Nothing named WEIJEFWEF has been defined." === ex.getMessage)
    }}

  test("Plot With Bad Pen Setup Code Should Throw Exception on Load (headless only)") { implicit fixture =>
    testCompileError(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), pens = List(Pen(display = "", setupCode = "create-fails 8")))))) { ex =>
      assert("Nothing named CREATE-FAILS has been defined." === ex.getMessage)
    }}

  test("Plot With Bad Pen Update Code Should Throw Exception on Load (headless only)") { implicit fixture =>
    testCompileError(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), pens = List(Pen(display = "p", updateCode = "create-fails 8")))))) { ex =>
      assert("Nothing named CREATE-FAILS has been defined." === ex.getMessage)
    }}

  test("Plot With Bad Pen Update Code Should Output Exception on Load (headless only, Output Option)") { implicit fixture =>
    import fixture._
    setPlotCompilationErrorAction(PlotCompilationErrorAction.Output)
    openModel(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), pens = List(Pen(display = "p", updateCode = "create-fails 8"))))))
  }

  test("Plot With Bad Pen Update Code Should Ignore Exception on Load (headless only, Ignore Option)") { implicit fixture =>
    import fixture._
    setPlotCompilationErrorAction(PlotCompilationErrorAction.Ignore)
    openModel(Model(code = modelCode, widgets = List(View(), Plot(display = Some(""), pens = List(Pen(display = "p", updateCode = "create-fails 8"))))))
  }
}
