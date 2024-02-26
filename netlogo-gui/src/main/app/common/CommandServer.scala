// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app.common

import org.nlogo.app.common.CommandLine
import org.nlogo.awt.EventQueue
import org.nlogo.core.{AgentKind, Widget => CoreWidget}
import org.nlogo.window.JobWidget
import org.nlogo.workspace.AbstractWorkspace

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

import java.net.ServerSocket
import java.io.{ BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter }

import scala.jdk.CollectionConverters._

private class CommandRequest(var agent: AgentKind = AgentKind.Observer, var proc: String = "", var args: Array[String] = Array()) {
  // TODO: Example: {"agent": "observer", "proc": "show", "args": ["1337"]}
  def fromJSONString(input: String): Boolean = {
    val parser = new JSONParser()
    var successful = false

    try {
      val obj = parser.parse(input).asInstanceOf[JSONObject];

      agent = obj.get("agent").asInstanceOf[String] match {
        case "observer" => AgentKind.Observer
        case "turtle" => AgentKind.Turtle
        case "patch" => AgentKind.Patch
        case "link" => AgentKind.Link
        case _ => throw Exception("Unknown agent kind")
      }

      proc = obj.get("proc").asInstanceOf[String]

      val jsonArgs = obj.get("args").asInstanceOf[JSONArray]
      args = jsonArgs.toArray.map(x => x.asInstanceOf[String])

      successful = true
    } catch {
      case _: Throwable => println("Failed to parse remote command")
    }

    successful
  }
}

private class CommandThread(serverSocket: ServerSocket, callback: CommandRequest => Unit) extends Thread {
  override def run() = {
    while (true) {
      val request = new CommandRequest
      val socket = serverSocket.accept
      val input = socket.getInputStream
      val reader = new BufferedReader(new InputStreamReader(input))
      val output = socket.getOutputStream
      val writer = new BufferedWriter(new OutputStreamWriter(output))

      for (line <- reader.lines.iterator.asScala) {
        if (request.fromJSONString(line)) {
          callback(request)
          writer.write(s"{\"status\": \"ok\"}\n")
        } else {
          writer.write(s"{\"status\": \"parsing failed\"}\n")
        }

        writer.flush()
      }

      input.close
      output.close
      socket.close
    }
  }
}

class CommandServer(commandLine: CommandLine, workspace: AbstractWorkspace)
  extends JobWidget(workspace.world.mainRNG) {

  private val serverSocket = new ServerSocket(4004)
  private val thread = new CommandThread(serverSocket, commandCallback)

  thread.start

  private def commandCallback(request: CommandRequest) = {
    val code = s"${request.proc} ${request.args.mkString(" ")}"

    EventQueue.invokeAndWait({() =>
      commandLine.execute(request.agent, code, true)
    })
  }

  override def syncTheme(): Unit = {
  }

  override def getEditable: Option[org.nlogo.window.Editable] = {
    None
  }
  override def load(widget: CoreWidget): Unit = {
    throw new UnsupportedOperationException()
  }

  override def model: CoreWidget = {
    throw new UnsupportedOperationException()
  }

  override def raiseWidgetRemoved(): Unit = {}
  override def raiseWidgetAdded(): Unit = {}
}
