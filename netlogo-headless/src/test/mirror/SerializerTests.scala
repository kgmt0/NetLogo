// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.mirror

import org.scalatest.funsuite.AnyFunSuite
import Serializer.{ toBytes, fromBytes }

class SerializerTests extends AnyFunSuite {

  def roundTrip(update: Update): Unit = {
    assertResult(update) { fromBytes(toBytes(update)) }
  }

  test("empty") {
    roundTrip(Update())
  }

  test("one death") {
    roundTrip(Update(deaths = Seq(Death(AgentKey(kind = Mirrorables.Turtle, id = 3)))))
  }

  test("one birth no values") {
    roundTrip(Update(births = Seq(Birth(AgentKey(kind = Mirrorables.Patch, id = 5),
                                        Seq()))))
  }

  test("one birth with values") {
    roundTrip(Update(births = Seq(Birth(AgentKey(kind = Mirrorables.Patch, id = 5),
                                        Seq("foo")))))
  }

  test("a change") {
    roundTrip(Update(changes = Seq((AgentKey(kind = Mirrorables.Link, id = 5),
                                    Seq(Change(3, "red"))))))
  }

}
