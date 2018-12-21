package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval

object ProficiencyBonus {

  type ProficiencyBonus = Int Refined Interval.ClosedOpen[W.`0`.T, W.`7`.T]

  // Taken from Player's Handbook page 15.
  def fromLevel(level: Level): ProficiencyBonus = level.value match {
    case 1 | 2 | 3 | 4     => 2
    case 5 | 6 | 7 | 8     => 3
    case 9 | 10 | 11 | 12  => 4
    case 13 | 14 | 15 | 16 => 5
    case 17 | 18 | 19 | 20 => 6
  }
}
