// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.OneInstancePerTest
import org.nlogo.core.WorldDimensions
import org.nlogo.api.{PlotInterface, ImporterUser}

class ImporterTests extends AnyFunSuite with OneInstancePerTest {
  val IGNORE_ERROR_HANDLER =
    new ImporterJ.ErrorHandler() {
      def showError(title: String, errorDetails: String, fatalError: Boolean) =
        // don't do anything and we always want to continue, so return true.
        true
    }
  val IMPORTER_USER =
    new ImporterUser() {
      def setDimensions(d: WorldDimensions): Unit = {
        world.createPatches(d.minPxcor, d.maxPxcor, d.minPycor, d.maxPycor)
      }
      def setDimensions(d: WorldDimensions, patchSize: Double): Unit = {
        world.createPatches(d.minPxcor, d.maxPxcor, d.minPycor, d.maxPycor)
        world.patchSize(patchSize)
      }
      def patchSize(patchSize: Double): Unit = {
        world.patchSize(patchSize)
      }
      def setOutputAreaContents(text: String): Unit = { }
      def resizeView(): Unit = { }
      def currentPlot(plot: String): Unit = { }
      def getPlot(plot: String): PlotInterface = null
      def importExtensionData(name: String, data: java.util.List[Array[String]], handler: org.nlogo.api.ImportErrorHandler): Unit = { }
      def isExtensionName(name: String) = false
    }
  class StringReaderTest extends ImporterJ.StringReader {
    def readFromString(s: String): AnyRef = {
      try Int.box(s.toInt)
      catch { case ex: NumberFormatException =>
        throw new ImporterJ.StringReaderException("invalid integer")
      }
    }
  }
  val world = new World2D()
  world.createPatches(-10, 10, -10, 10)
  world.realloc()
  val importer = new Importer(IGNORE_ERROR_HANDLER, world,
                              IMPORTER_USER, new StringReaderTest())
  def testGetTokenValue(testString: String, turtleBreedVar: Boolean, expected: AnyRef): Unit = {
    val result = importer.getTokenValue(testString, turtleBreedVar, false)
    assertResult(expected)(result)
  }
  def testInvalidGetTokenValue(testString: String, turtleBreedVar: Boolean): Unit = {
    val result = importer.getTokenValue(testString, turtleBreedVar, false)
    if(turtleBreedVar)
      assertResult(world.turtles)(result)
    else
      assert(result.isInstanceOf[ImporterJ.Junk])
  }

  test("Jikes115BugTest1") {
    testGetTokenValue("5", false, Int.box(5))
  }
  test("Jikes115BugTest2") {
    testGetTokenValue("5", true, Int.box(5))
  }
  test("Jikes115BugInvalidTest1") {
    testInvalidGetTokenValue("23fish", false)
  }
  test("Jikes115BugInvalidTest2") {
    testInvalidGetTokenValue("23fish", true)
  }

}
