// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim

import org.nlogo.agent.{ Patch, Turtle }
import org.nlogo.api.AgentException
import org.nlogo.nvm.{ Context, Reporter }
import org.nlogo.nvm.RuntimePrimitiveException

class _patchvariabledouble(vnInit: Int) extends Reporter {
  // this used to be inlined in the constructor, but that confused MethodRipper for some reason (Isaac B 5/9/25)
  private var _vn = vnInit

  def this() = this(0)

  override def toString =
    super.toString + ":" +
      Option(world).map(_.patchesOwnNameAt(vn)).getOrElse(vn.toString)

  // MethodRipper won't let us call a public method from report_1()
  // so we must keep vn and _vn separate - ST 9/22/12
  def vn = _vn
  def vn_=(vn: Int): Unit = { _vn = vn }

  override def report(context: Context): java.lang.Double =
    report_1(context)

  def report_1(context: Context): java.lang.Double =
    try context.agent.getPatchVariable(_vn).asInstanceOf[java.lang.Double]
    catch { case ex: AgentException =>
      throw new RuntimePrimitiveException(context, this, ex.getMessage) }

  def report_2(context: Context): Double = {
    val patch = context.agent match {
      case t: Turtle => t.getPatchHere
      case p: Patch => p
      case a => throw new IllegalStateException
    }
    patch.getPatchVariableDouble(_vn)
  }

}
