package io.github.tjheslin1.dmspredictor.classes.cleric

import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

abstract class BaseCleric extends Player with SpellCaster {

  val spellCastingModifier     = proficiencyBonus.value
  val levelSpellcastingLearned = LevelOne

  val channelDivinityUsed: Boolean

  def resetStartOfTurn(): Creature = this

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object BaseCleric {

  val HitDice = D8

  def calculateHealth(level: Level, constitutionScore: Stat): Int =
    Player.calculateHealth(HitDice, level, constitutionScore)

  def calculateArmourClass(stats: BaseStats, armour: Armour, offHand: Option[Equipment]): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus = offHand match {
      case Some(Shield) => Shield.armourClass(stats.dexterity)
      case _            => 0
    }

    baseArmourClass + shieldBonus
  }

  val channelDivinityUsedLens: Lens[BaseCleric, Boolean] =
    Lens[BaseCleric, Boolean](_.channelDivinityUsed) { channelDivinityUsed =>
      {
        case cleric: Cleric => Cleric._channelDivinityUsed.set(channelDivinityUsed)(cleric)

        case _ => throw new NotImplementedError("Missing a case in channelDivinityUsedLens")

      }
    }
}
