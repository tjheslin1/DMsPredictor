package io.github.tjheslin1.dmspredictor.classes.wizard

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._

trait BaseWizard extends Player with Product with Serializable {

  val mageArmourPrepared: Boolean

  def resetStartOfTurn(): Creature       = this
  def scoresCritical(roll: Int): Boolean = roll == 20
}

object BaseWizard {

  val HitDice = D6

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    Player.calculateHealth(HitDice, level, constitutionScore)

  def calculateArmourClass(stats: BaseStats, mageArmourPrepared: Boolean): Int =
    if (mageArmourPrepared) 13 + mod(stats.dexterity)
    else 10 + mod(stats.dexterity)
}
