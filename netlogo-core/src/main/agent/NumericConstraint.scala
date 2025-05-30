// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.nlogo.api.ValueConstraint

/**
 * Constraint suitable for Slider variables.
 */
class NumericConstraint(var defaultValue: java.lang.Double) extends ValueConstraint {

  def this() = this(World.Zero)

  @throws(classOf[ValueConstraint.Violation])
  def assertConstraint(value: AnyRef): Unit = {
    if(!value.isInstanceOf[java.lang.Double])
      throw new ValueConstraint.Violation("Value must be a number.")
  }

  def coerceValue(value: AnyRef): java.lang.Double =
    value match {
      case d: java.lang.Double => d
      case s: String => java.lang.Double.valueOf(s)  // ugh, is this ever used? - ST 5/6/13
      case _ => defaultValue
    }

}
