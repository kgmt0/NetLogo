// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.compile

import org.scalatest.funsuite.AnyFunSuite
import org.nlogo.api.Version.useGenerator
import org.nlogo.api.{ DummyExtensionManager, DummyLibraryManager }
import org.nlogo.core.{ DummyCompilationEnvironment, Program }
import org.nlogo.nvm.Procedure.NoProcedures

class TestSourcePositions extends AnyFunSuite {
  val program = Program.empty()
  def compileReporter(source: String) =
    Compiler.compileMoreCode(
      "to foo __ignore " + source + "\nend", None, program,
      NoProcedures,
      new DummyExtensionManager, new DummyLibraryManager, new DummyCompilationEnvironment
    ).head.code.head.args.head.fullSource
  def compileCommand(source: String) =
    Compiler.compileMoreCode(
      "to foo " + source + "\nend", None, program,
      NoProcedures,
      new DummyExtensionManager, new DummyLibraryManager, new DummyCompilationEnvironment
    ).head.code.head.fullSource
  def reporter(s: String): Unit = { assertResult(s)(compileReporter(s)) }
  def command(s: String): Unit = { assertResult(s)(compileCommand(s)) }
  def command(expected: String, s: String): Unit = { assertResult(expected)(compileCommand(s)) }
  if (useGenerator) {
    /// reporters
    test("one") { reporter("timer") }
    test("many") { reporter("timer + timer + timer + timer + timer") }
    test("less") { reporter("timer < timer") }
    test("int") { reporter("3") }
    test("string") { reporter("\"foo\"") }
    test("constantFolding") { reporter("2 + 2") }
    test("reporterlambda") { reporter("[[] -> 2 + 2 ]") }
    /// commands
    test("iffy") { command("if timer < 10 [ print timer ]", "if timer < 10 [ print timer ]") }
    test("lambdas") { command("run [[] -> fd 1 ]") }
    test("repeat") { command("repeat 3 [ ca ]") }
    // parens are omitted. fixing this seems hard - ST 2/12/09, RG 5/31/16
    test("parens") { command("fd 3", "fd (3)") }
  }
}
