package io.github.tjheslin1.dmspredictor.classes.wizard

import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.ShieldBuffCondition

trait BaseWizard extends Player with SpellCaster with Product with Serializable {

  val spellCastingModifier  = proficiencyBonus.value
  val spellCastingAttribute = Intelligence

  val spellCastingLevel        = level
  val levelSpellcastingLearned = LevelOne

  val mageArmourPrepared: Boolean

  def resetStartOfTurn(): Creature       = this
  def scoresCritical(roll: Int): Boolean = roll == 20
}

object BaseWizard {

  val HitDice = D6

  def calculateHealth(level: Level, constitutionScore: Stat): Int = Player.calculateHealth(
    HitDice,
    level,
    constitutionScore)

  def calculateArmourClass(
      stats: BaseStats,
      mageArmourPrepared: Boolean,
      conditions: List[Condition]
  ): Int = {
    val armourClass =
      if (mageArmourPrepared)
        13 + mod(stats.dexterity)
      else
        10 + mod(stats.dexterity)

    if (conditions.contains(ShieldBuffCondition()))
      armourClass + 5
    else
      armourClass
  }
}
