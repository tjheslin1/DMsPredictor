package io.github.tjheslin1.model

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.model.BaseStats.Stat

case class BaseStats(strength: Stat,
                     dexterity: Stat,
                     constitution: Stat,
                     wisdom: Stat,
                     intelligence: Stat,
                     charisma: Stat)

object BaseStats {
  type Stat = Int Refined Interval.ClosedOpen[W.`1`.T, W.`31`.T]
}
