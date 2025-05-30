// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim

import org.nlogo.nvm.{ Command, Context }

class _return extends Command {

  // for use in error messages
  override def displayName =
    "END"

  override def perform(context: Context): Unit = {
    perform_1(context)
  }

  def perform_1(context: Context): Unit = {
    context.returnFromProcedure()
    // see _stop.perform() for commentary on this - ST 7/15/04
    context.stopping = false
  }

  def profiling_perform_1(context: Context): Unit = {
    // profiling data collection, close out the call record
    workspace.profilingTracer.closeCallRecord(context, context.activation)
    context.returnFromProcedure()
    // see _stop.perform() for commentary on this - ST 7/15/04
    context.stopping = false
  }

}
