// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim;

import org.nlogo.agent.AgentIterator;
import org.nlogo.core.AgentKindJ;
import org.nlogo.agent.AgentSet;
import org.nlogo.agent.Patch;
import org.nlogo.agent.Turtle;
import org.nlogo.core.I18N;
import org.nlogo.api.LogoException;
import org.nlogo.core.Syntax;
import org.nlogo.nvm.ArgumentTypeException;
import org.nlogo.nvm.RuntimePrimitiveException;
import org.nlogo.nvm.Reporter;

import java.util.ArrayList;
import java.util.List;

public final class _turtleson
  extends Reporter {

  @Override
  public Object report(final org.nlogo.nvm.Context context) throws LogoException {
    Object agentOrSet = args[0].report(context);
    List<Turtle> resultList = new ArrayList<Turtle>();
    if (agentOrSet instanceof Turtle) {
      Turtle turtle = (Turtle) agentOrSet;
      if (turtle.id() == -1) {
        throw new RuntimePrimitiveException(context, this,
          I18N.errorsJ().getN("org.nlogo.$common.thatAgentIsDead", turtle.classDisplayName()));
      }
      addAll(resultList, turtle.getPatchHere().turtlesHere());

    } else if (agentOrSet instanceof Patch) {
      addAll(resultList, ((Patch) agentOrSet).turtlesHere());

    } else if (agentOrSet instanceof AgentSet) {
      AgentSet sourceSet = (AgentSet) agentOrSet;
      if (sourceSet.kind() == AgentKindJ.Turtle()) {
        for (AgentIterator iter = sourceSet.iterator(); iter.hasNext();) {
          addAll(resultList, ((Turtle) iter.next()).getPatchHere().turtlesHere());
        }
      } else {
        for (AgentIterator iter = sourceSet.iterator(); iter.hasNext();) {
          addAll(resultList, ((Patch) iter.next()).turtlesHere());
        }
      }

    } else {
      throw new ArgumentTypeException
          (context, this, 0, Syntax.AgentType() | Syntax.AgentsetType(), agentOrSet);
    }

    return AgentSet.fromArray(AgentKindJ.Turtle(), resultList.toArray(new Turtle[resultList.size()]));
  }

  private void addAll(List<Turtle> turtles, Iterable<Turtle> moreTurtles) {
    for (Turtle t : moreTurtles) {
      if(!turtles.contains(t))
        turtles.add(t);
    }
  }
}
