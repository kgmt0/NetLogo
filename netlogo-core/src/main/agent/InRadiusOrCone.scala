// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.agent

import org.nlogo.api.AgentException
import org.nlogo.core.AgentKind
import java.util.{ArrayList, LinkedHashSet, HashSet => JHashSet, List => JList}

class InRadiusOrCone private[agent](val world: World2D) extends World.InRadiusOrCone {
  private var patches: Array[Patch] = null // world.patches is not defined when this class is initiated
  private var end: Int = 0

  override def inRadius(agent: Agent, sourceSet: AgentSet, radius: Double, wrap: Boolean): JList[Agent] = {
    val (worldWidth, worldHeight) = (world.worldWidth, world.worldHeight)
    val result = new ArrayList[Agent]
    var gRoot = .0
    var dx, dy, i = 0
    val centerPatch = agent match {
      case t: Turtle => t.currentPatch
      case p: Patch => p
      case _ => throw new IllegalStateException
    }
    // set agent coordinates startX, startY, patchX, patchY
    val (startX, startY, patchX, patchY) = agent match {
      case t: Turtle => (t.xcor, t.ycor, centerPatch.pxcor, centerPatch.pycor)
      case p: Patch => (p.pxcor.toDouble, p.pycor.toDouble, p.pxcor, p.pycor)
      case _ => throw new IllegalStateException
    }

    val cachedIDs = initCachedIDs(sourceSet)
    initPatches()
    if (radius <= 1 && radius > 0) { // use getNeighbors to get patches in this case
      setPatches1(centerPatch)
    } else if (radius <= 2) {
      setPatches2(centerPatch)
    } else {
      setPatches(startX, startY, radius)
    }

    if (sourceSet.kind eq AgentKind.Patch) {
      val sourceSetIsWorldPatches = sourceSet eq world.patches

      while (i < end) {
        val patch = patches(i)

        if ((sourceSetIsWorldPatches || cachedIDs.contains(patch.id))
          && world.protractor.distance(patch.pxcor, patch.pycor, startX, startY, wrap) <= radius) {
          result.add(patch)
        }

        i += 1
      }
    } else if (sourceSet.kind eq AgentKind.Turtle) {
      val sourceSetIsWorldTurtles = sourceSet eq world.turtles
      val sourceSetIsBreedSet = sourceSet.isBreedSet
      val worldWidth2 = worldWidth / 2
      val worldHeight2 = worldHeight / 2

      while (i < end) {
        val patch = patches(i)

        dx = Math.abs(patch.pxcor - patchX)
        if (world.topology.xWraps && dx > worldWidth2)
          dx = worldWidth - dx

        dy = Math.abs(patch.pycor - patchY)
        if (world.topology.yWraps && dy > worldHeight2)
          dy = worldHeight - dy

        gRoot = world.rootsTable.gridRoot(dx * dx + dy * dy)

        // The 1.415 (square root of 2) adjustment is necessary because it is
        // possible for portions of a patch to be within the circle even though
        // the center of the patch is outside the circle.  Both turtles, the
        // turtle in the center and the turtle in the agentset, can be as much
        // as half the square root of 2 away from its patch center.  If they're
        // away from the patch centers in opposite directions, that makes a total
        // of square root of 2 additional distance we need to take into account.
        if (gRoot <= radius + 1.415) {
          val turtleIterator = patch.turtlesHere.iterator
          while (turtleIterator.hasNext) {
            val turtle = turtleIterator.next()
            if ((sourceSetIsWorldTurtles
              || (sourceSetIsBreedSet && (sourceSet eq turtle.getBreed))
              || cachedIDs.contains(turtle.id))
              && (gRoot <= radius - 1.415
              || world.protractor.distance(turtle.xcor, turtle.ycor, startX, startY, wrap) <= radius))
              result.add(turtle)
          }
        }
        i += 1
      }
    }

    result
  }

  override def inCone(startTurtle: Turtle, sourceSet: AgentSet, radius: Double, angle: Double, wrap: Boolean): JList[Agent] = {
    val (worldWidth, worldHeight) = (world.worldWidth, world.worldHeight)
    var m, n = 0
    // If wrap is true and the radius is large enough, the cone
    // may wrap around the edges of the world.  We handle this by
    // enlarging the coordinate system in which we search beyond
    // the world edges and then filling the enlarged coordinate
    // system with "copies" of the world.  At least, you can
    // imagine it that way; we don't actually copy anything.  m
    // and n are the maximum number of times the cone might wrap
    // around the edge of the world in the X and Y directions, so
    // that's how many world copies we will need to make.  The
    // copies will range from -m to +m on the x axis and -n to +n
    // on the y axis.
    if (wrap) {
      if (world.wrappingAllowedInX)
        m = StrictMath.ceil(radius / worldWidth).toInt

      if (world.wrappingAllowedInY)
        n = StrictMath.ceil(radius / worldHeight).toInt
    }
    // in the nonwrapping case, we don't need any world copies besides
    // the original, so we have only one pair of offsets and both of
    // them are 0

    val result = new ArrayList[Agent]
    val half: Double = angle / 2
    var gRoot: Double = .0
    var dx, dy, i = 0

    val cachedIDs = initCachedIDs(sourceSet)
    initPatches()
    if (radius <= 1 && radius > 0) { // use getNeighbors to get patches in this case
      setPatches1(startTurtle.currentPatch)
    } else if (radius <= 2) {
      setPatches2(startTurtle.currentPatch)
    } else {
      setPatches(startTurtle.xcor, startTurtle.ycor, radius)
    }

    // create and filter world offsets based on startTurtle position.
    // x offset from -m to m and y offset from -n to n.
    val offsets = new ArrayList[(Int, Int)]
    var x = -m
    val looseRadius = StrictMath.ceil(radius).toInt + 1
    while (x <= m) {
      var y = -n
      while (y <= n) {
        if (closestPointIsInRadius(startTurtle.xcor, startTurtle.ycor, x, y, looseRadius)) {
          offsets.add((x, y))
        }
        y += 1
      }
      x += 1
    }

    if (sourceSet.kind eq AgentKind.Patch) {
      val sourceSetIsWorldPatches = sourceSet eq world.patches

      while (i < end) {
        val patch = patches(i)

        if (patch != null) {
          // loop through the patches in the rectangle.  (it doesn't matter what
          // order we check them in.)
          val offsetIterator = offsets.iterator
          var found = false
          while (!found && offsetIterator.hasNext) {
            val (offsetX, offsetY) = offsetIterator.next()

            if ((sourceSetIsWorldPatches || cachedIDs.contains(patch.id))
              && isInCone(patch.pxcor + worldWidth * offsetX, patch.pycor + worldHeight * offsetY, startTurtle.xcor, startTurtle.ycor, radius, half, startTurtle.heading)) {
              result.add(patch)
              found = true
            }
          }
        }
        i += 1
      }
    } else {
      val sourceSetIsWorldTurtles = sourceSet eq world.turtles
      val sourceSetIsBreedSet = sourceSet.isBreedSet
      val worldWidth2 = worldWidth / 2
      val worldHeight2 = worldHeight / 2

      while (i < end) {
        val patch = patches(i)

        dx = Math.abs(patch.pxcor - startTurtle.currentPatch.pxcor)
        if (dx > worldWidth2)
          dx = worldWidth - dx

        dy = Math.abs(patch.pycor - startTurtle.currentPatch.pycor)
        if (dy > worldHeight2)
          dy = worldHeight - dy

        gRoot = world.rootsTable.gridRoot(dx * dx + dy * dy)

        // Only check patches that might have turtles within the radius on them.
        // The 1.415 (square root of 2) adjustment is necessary because it is
        // possible for portions of a patch to be within the circle even though
        // the center of the patch is outside the circle.  Both turtles, the
        // turtle in the center and the turtle in the agentset, can be as much
        // as half the square root of 2 away from its patch center.  If they're
        // away from the patch centers in opposite directions, that makes a total
        // of square root of 2 additional distance we need to take into account.
        if (gRoot <= radius + 1.415) {
          val turtleIterator = patch.turtlesHere.iterator
          while (turtleIterator.hasNext) {
            val turtle = turtleIterator.next()

            var found = false
            val offsetIterator = offsets.iterator
            while (!found && offsetIterator.hasNext) {
              val (offsetX, offsetY) = offsetIterator.next()
              if ((sourceSetIsWorldTurtles || (sourceSetIsBreedSet && (sourceSet eq turtle.getBreed))
                || cachedIDs.contains(turtle.id))
                && isInCone(turtle.xcor + worldWidth * offsetX, turtle.ycor + worldHeight * offsetY, startTurtle.xcor, startTurtle.ycor, radius, half, startTurtle.heading)) {
                result.add(turtle)
                found = true
              }
            }
          }
        }
        i += 1
      }
    }

    result
  }

  // for each world W at offsets offsetX and offsetY, check if any point
  // in W is within radius by checking W's closest point
  @scala.inline
  private def closestPointIsInRadius(x: Double, y: Double, offsetX: Int, offsetY: Int, r: Double): Boolean = {
    var closestX, closestY = .0

    if (offsetY < 0) { // below current world
      closestY = world.maxPycor + (offsetY * world.worldHeight)
    } else if (offsetY > 0) { // above current world
      closestY = world.minPycor + (offsetY * world.worldHeight)
    } else { // same horizontal as current world
      closestY = y
    }

    if (offsetX < 0) { // left of current world
      closestX = world.maxPxcor + (offsetX * world.worldWidth)
    } else if (offsetX > 0) { //right of current world
      closestX = world.minPxcor + (offsetX * world.worldWidth)
    } else { // same vertical as current world
      closestX = x
    }

    world.protractor.distance(closestX, closestY, x, y, false) <= r
  }

  // check if (x, y) is in the cone with center (cx, cy) , radius r, half-angle half, and central
  // line of the cone having heading h.
  @scala.inline
  private def isInCone(x: Double, y: Double, cx: Double, cy: Double, r: Double, half: Double, h: Double): Boolean = {
    if (x == cx && y == cy) {
      return true
    }
    if (world.protractor.distance(cx, cy, x, y, false) > r) { // false = don't wrap, since inCone()
      // handles wrapping its own way
      return false
    }
    var theta = .0
    try {
      theta = world.protractor.towards(cx, cy, x, y, false)
    } catch {
      case e: AgentException =>
        // this should never happen because towards() only throws an AgentException
        // when the distance is 0, but we already ruled out that case above
        throw new IllegalStateException(e.toString)
    }
    val diff = StrictMath.abs(theta - h)
    // we have to be careful here because e.g. the difference between 5 and 355
    // is 10 not 350... hence the 360 thing
    (diff <= half) || ((360 - diff) <= half)
  }

  @scala.inline
  private def initPatches(): Unit = {
    if ((patches eq null) || patches.length != world.patches.count) {
      patches = new Array[Patch](world.patches.count)
    }
  }

  // special case of setPatches for radius <= 1
  @scala.inline
  private def setPatches1(centerPatch: Patch): Unit = {
    val neighbors = centerPatch.getNeighbors.asInstanceOf[ArrayAgentSet].array
    var i = 0

    patches(0) = centerPatch
    while (i < neighbors.length) {
      patches(i + 1) = neighbors(i).asInstanceOf[Patch]
      i += 1
    }

    end = i + 1
  }

  // special case of setPatches for radius <= 2
  @scala.inline
  private def setPatches2(centerPatch: Patch): Unit = {
    setPatches1(centerPatch)

    // Special case for when duplicates are possible.
    // In such a case, use hash set to account for duplicates.
    // This is necessary when:
    // - world is tiny in some dimension with wrapping
    // - center is too close to edge with no wrapping
    // EH 3/1/18
    if ((world.wrappingAllowedInX && world.worldWidth < 5) || (world.wrappingAllowedInY && world.worldHeight < 5)
      || (!world.wrappingAllowedInX && (centerPatch.pxcor - world.minPxcor < 2 || world.maxPxcor - centerPatch.pxcor < 2))
      || (!world.wrappingAllowedInY && (centerPatch.pycor - world.minPycor < 2 || world.maxPycor - centerPatch.pycor < 2))) {

      // because of how we're adding to allNeighbors, there should never be any duplicates, except
      // when the world is very small.
      val allNeighbors = new LinkedHashSet[Patch]

      var i = 0
      while (i < end) {
        val p = patches(i)
        allNeighbors.add(p)

        allNeighbors.add(p.getPatchNorth)
        allNeighbors.add(p.getPatchNorthEast)
        allNeighbors.add(p.getPatchEast)
        allNeighbors.add(p.getPatchSouthEast)

        allNeighbors.add(p.getPatchSouth)
        allNeighbors.add(p.getPatchSouthWest)
        allNeighbors.add(p.getPatchWest)
        allNeighbors.add(p.getPatchNorthWest)

        i += 1
      }

      allNeighbors.remove(null)

      i = 0
      val allNeighborsIterator = allNeighbors.iterator
      while (allNeighborsIterator.hasNext) {
        patches(i) = allNeighborsIterator.next()
        i += 1
      }

      end = i
    } else {
      patches(9) = patches(1).getPatchNorth
      patches(10) = patches(2).getPatchEast
      patches(11) = patches(3).getPatchSouth
      patches(12) = patches(4).getPatchWest

      patches(13) = patches(5).getPatchNorth
      patches(14) = patches(5).getPatchNorthEast
      patches(15) = patches(5).getPatchEast

      patches(16) = patches(6).getPatchEast
      patches(17) = patches(6).getPatchSouthEast
      patches(18) = patches(6).getPatchSouth

      patches(19) = patches(7).getPatchSouth
      patches(20) = patches(7).getPatchSouthWest
      patches(21) = patches(7).getPatchWest

      patches(22) = patches(8).getPatchWest
      patches(23) = patches(8).getPatchNorthWest
      patches(24) = patches(8).getPatchNorth

      end = 25
    }
  }

  @scala.inline
  private def setPatches(X: Double, Y: Double, R: Double): Unit = {
    // initialize patches only once for this class.

    val x = StrictMath.round(X).toInt
    val y = StrictMath.round(Y).toInt
    val r = R.toInt + 1

    val regionIterator = world.topology.getRegion(x, y, r).iterator
    var length, curr = 0
    var region = (0, 0)

    val worldPatches = world.patches.asInstanceOf[ArrayAgentSet].array

    while (regionIterator.hasNext) {
      region = regionIterator.next()
      length = region._2 - region._1
      System.arraycopy(worldPatches, region._1, patches, curr, length)
      curr += length
    }

    end = curr
  }

  @scala.inline
  private def initCachedIDs(sourceSet: AgentSet): JHashSet[Long] = {
    var cachedIDs: JHashSet[Long] = null
    if (!sourceSet.isBreedSet) {
      cachedIDs = new JHashSet[Long](sourceSet.count)
      val sourceTurtles = sourceSet.iterator
      while (sourceTurtles.hasNext) {
        val t = sourceTurtles.next()
        cachedIDs.add(t.id)
      }
    } else {
      cachedIDs = new JHashSet[Long](0)
    }

    cachedIDs
  }
}
