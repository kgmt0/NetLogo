// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim

import org.nlogo.nvm.{ Command, Context, MutableLong }

class _repeatlocalinternal(vn: Int, _offset: Int) extends Command {

  offset = _offset

  override def toString =
    super.toString + ":" + offset + "," + vn

  override def perform(context: Context): Unit = {
    perform_1(context)
  }

  def perform_1(context: Context): Unit = {
    val counter = context.activation.args(vn).asInstanceOf[MutableLong]
    if (counter.value <= 0)
      context.ip = next
    else {
      counter.value -= 1
      context.ip = offset
    }
  }

}
