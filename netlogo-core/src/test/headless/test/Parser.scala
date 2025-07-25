// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.headless.test

import java.util.Locale

import org.nlogo.core.{AgentKind, Keywords}

object Parser {

  def parse(suiteName: String, s: String): List[LanguageTest] = {
    def split(xs: List[String]): List[LanguageTest] =
      if (xs.isEmpty) Nil
      else xs.tail.span(_.startsWith(" ")) match {
        case (some, rest) =>
          LanguageTest(suiteName, xs.head.trim, some.map{_.trim}.map(parse)) :: split(rest)
      }
    val multiLine =
      s.split("/\r?\n")
        .map(_.trim)
        .mkString(" ")
    val lines =
      multiLine.split("\r?\n")
        .filter(!_.trim.startsWith("#"))
        .filter(!_.trim.isEmpty)
    split(lines.toList)
  }

  val CommandErrorRegex = """^([OTPL])>\s+(.*)\s+=>\s+(.*)$""".r
  val ReporterRegex = """^(.*)\s+=>\s+(.*)$""".r
  val CommandRegex = """^([OTPL])>\s+(.*)$""".r
  val OpenRegex = """^OPEN>\s+(.*)$""".r
  val CompileRegex = """^COMPILE>\s+(.*)$""".r

  def agentKind(s: String) = s match {
    case "O" => AgentKind.Observer
    case "T" => AgentKind.Turtle
    case "P" => AgentKind.Patch
    case "L" => AgentKind.Link
    case x =>
      throw new IllegalArgumentException(
        s"unrecognized agent kind: $x")
  }

  private def startsWithStrip(start: String, f: String => Result)(actual: String): Option[Result] =
    if (actual.startsWith(start))
      Some(f(actual.stripPrefix(start)))
    else
      None

  val errorParsers: Seq[String => Option[Result]] =
    Seq(startsWithStrip("ERROR ", RuntimeError(_)),
        startsWithStrip("COMPILER ERROR ", CompileError(_)),
        startsWithStrip("STACKTRACE ", (s: String) => StackTrace(s.replace("\\n", "\n"))))

  def parse(line: String): Entry = {
    if (line.split(' ').headOption.exists(s =>
        Keywords.isKeyword(s) || s.toUpperCase(Locale.ENGLISH) == "BREED"))
      Declaration(line)
    else line.trim match {
      case CommandErrorRegex(kind, command, err) =>
        errorParsers.flatMap(_(err)).headOption
          .map(Command(command, agentKind(kind), _))
          .getOrElse(throw new IllegalArgumentException(s"error missing!: $err"))
      case ReporterRegex(reporter, result) =>
        errorParsers.flatMap(_(result)).headOption
          .map(Reporter(reporter, _))
          .getOrElse(Reporter(reporter, Success(result)))
      case CommandRegex(kind, command) =>
        Command(command, agentKind(kind))
      case OpenRegex(path) => Open(path)
      case CompileRegex(err) =>
        errorParsers.flatMap(_(err)).headOption
          .map(Compile(_))
          .getOrElse(throw new IllegalArgumentException(s"error missing!: $err"))
      case _ =>
        throw new IllegalArgumentException(
          s"could not parse: $line")
    }
  }

}
