// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.api.Perspective
import org.nlogo.nvm.{ Command, Context }

class _rideme extends Command {
  switches = true
  override def perform(context: Context): Unit = {
    world.observer.setPerspective(Perspective.Ride(context.agent))
    context.ip = next
  }
}
