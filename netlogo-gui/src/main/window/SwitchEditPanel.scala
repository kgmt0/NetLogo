// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.window

import java.awt.{ GridBagConstraints, Insets }

import org.nlogo.api.{ CompilerServices, ExtensionManager }
import org.nlogo.core.I18N

class SwitchEditPanel(target: SwitchWidget, compiler: CompilerServices, extensionManager: ExtensionManager)
  extends WidgetEditPanel(target) {

  private val name =
    new IdentifierEditor(
      PropertyAccessor(
        target,
        I18N.gui.get("edit.switch.globalVar"),
        () => target.nameWrapper,
        name => target.setNameWrapper(name.getOrElse("")),
        () => apply()),
      compiler, extensionManager)

  private val oldSize =
    new BooleanEditor(
      PropertyAccessor(
        target,
        I18N.gui.get("edit.general.oldSize"),
        () => target.oldSize,
        _.foreach(target.oldSize),
        () => apply()))

  locally {
    val c = new GridBagConstraints

    c.gridx = 0
    c.anchor = GridBagConstraints.WEST
    c.fill = GridBagConstraints.HORIZONTAL
    c.weightx = 1
    c.insets = new Insets(6, 6, 6, 6)

    add(name, c)

    c.insets = new Insets(0, 6, 6, 6)

    add(oldSize, c)
  }

  override def propertyEditors: Seq[PropertyEditor[?]] =
    Seq(name, oldSize)

  override def requestFocus(): Unit = {
    name.requestFocus()
  }
}
