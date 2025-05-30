// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.nlogo.core.{ I18N, LogoList }
import org.nlogo.api.{ Dump, Equality, ValueConstraint }

class ChooserConstraint(private var _acceptedValues: LogoList = LogoList(),
                        var defaultIndex: Int = 0)
extends ValueConstraint
{
  def acceptedValues = _acceptedValues
  def acceptedValues(vals: LogoList): Unit = {
    val newdef = indexForValue(defaultValue).max(0)
    _acceptedValues = vals
    defaultIndex = newdef
  }

  // returns AnyRef since this is to satisfy the ValueConstraint interface
  def defaultValue: AnyRef =
    // empty when they start up before they've input anything, and also maybe when they put in
    // something unparsable from a file.
    if(_acceptedValues.isEmpty) ""
    else acceptedValues.get(defaultIndex.min(acceptedValues.size).max(0))

  def indexForValue(value: AnyRef): Int =
    acceptedValues.indexWhere(Equality.equals(_, value))

  @throws(classOf[ValueConstraint.Violation])
  def assertConstraint(value: AnyRef): Unit = {
    if(!acceptedValues.contains(value))
      throw new ValueConstraint.Violation(
        I18N.errors.getN("org.nlogo.agent.ChooserConstraint.invalidValue", Dump.logoObject(acceptedValues, true, false))
        )
  }

  def coerceValue(value: AnyRef): AnyRef =
    if(indexForValue(value) != -1)
      value
    else
      acceptedValues.first

}
