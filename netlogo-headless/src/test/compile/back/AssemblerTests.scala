// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.compile
package back

import org.scalatest.funsuite.AnyFunSuite
import org.nlogo.nvm

class AssemblerTests extends AnyFunSuite {

  def compile(keyword: String, source: String): nvm.Procedure = {
    val defs = Scaffold.apply(
      keyword + " foo " + source + "\nend")
    assertResult(1)(defs.size)
    for (procdef <- defs) {
      procdef.accept(new ArgumentStuffer)
      procdef.procedure.displayName = "procedure FOO"
      new Assembler().assemble(procdef)
    }
    defs.head.procedure
  }

  // these tests focus more on assembly, ignoring argument stuffing.
  // the test strings omit arguments, to make them easier to read & write.
  def test1(source: String) = compile("to", source).code.mkString(" ")
  test("assembleEmptyProcedure") { assertResult("_return")(test1("")) }
  test("assembleSimpleProcedure1") { assertResult("_clearall _return")(test1("ca")) }
  test("assembleSimpleProcedure2") { assertResult("_clearturtles _clearpatches _return")(test1("ct cp")) }
  test("assembleIfElse") {
    assertResult("_ifelse:+4 _fd _fdinternal _goto:6 _bk _fdinternal _return")(
      test1("ifelse timer = 0 [ fd 1 ] [ bk 1 ]"))
  }
  test("assembleAsk") {
    assertResult("_ask:+3 _die _done _return")(
      test1("ask turtles [ die ]"))
  }
  test("assembleWhile") {
    assertResult("_goto:3 _die _die _while:1 _return")(
      test1("while [true] [die die]"))
  }
  test("assembleReporterProcedure") {
    assertResult("_report _returnreport")(
      compile("to-report", "report 10").code.mkString(" "))
  }

  // these tests are more about checking argument stuffing is working.
  // we check the full text of the procedure dump.
  def test2(source: String): String = {
    val dump = compile("to", source).dump
    val prelude = "procedure FOO:[]{OTPL}:\n"
    assert(dump.startsWith(prelude))
    dump.substring(prelude.length)
  }
  test("stuffEmpty") {
    assertResult("""|[0]_return
              |""".stripMargin.replaceAll("\r\n", "\n"))(
      test2(""))
  }
  test("stuffArithmetic") {
    assertResult("""|[0]_print
           |      _plus
           |        _const:2
           |        _const:2
           |[1]_return
           |""".stripMargin.replaceAll("\r\n", "\n"))(
      test2("print 2 + 2"))
  }
  test("stuffReporterBlock") {
    assertResult("""|[0]_print
           |      _maxoneof
           |        _turtles
           |        _timer
           |[1]_return
           |""".stripMargin.replaceAll("\r\n", "\n"))(
      test2("print max-one-of turtles [timer]"))
  }
}
