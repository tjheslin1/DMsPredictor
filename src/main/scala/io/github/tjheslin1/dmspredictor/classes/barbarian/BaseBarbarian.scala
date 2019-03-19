package io.github.tjheslin1.dmspredictor.classes.barbarian

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian.resetStatus
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait BaseBarbarian extends Player with Product with Serializable {

  val inRage: Boolean
  val rageUsages: Int
  val rageTurnsLeft: Int

  def resetStartOfTurn(): Creature = resetStatus(this)
}

object BaseBarbarian {

  val HitDice = D12

  val rageUsagesPerLevel: Map[Level, Int] = Map(
    LevelOne   -> 2,
    LevelTwo   -> 2,
    LevelThree -> 3,
    LevelFour  -> 3,
    LevelFive  -> 3
  )

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    (D12.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(HitDice) + mod(
      constitutionScore)))

  def weaponWithRageDamage[_: RS](weapon: Weapon, inRage: Boolean): Weapon = {
    lazy val inRageDamage = if (inRage) weapon.damage + 2 else weapon.damage

    Weapon(weapon.name,
           weapon.weaponType,
           weapon.damageType,
           weapon.twoHanded,
           inRageDamage,
           weapon.hitBonus)
  }

  def calculateArmourClass(stats: BaseStats, armour: Armour, offHand: Option[Equipment]): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus = offHand match {
      case Some(Shield) => Shield.armourClass(stats.dexterity)
      case _            => 0
    }

    armour match {
      case NoArmour => baseArmourClass + mod(stats.constitution) + shieldBonus // Unarmoured Defense
      case _        => baseArmourClass + shieldBonus
    }
  }

  def resetStatus(baseBarbarian: BaseBarbarian): BaseBarbarian = {
    val resetAttackStatus = Creature.creatureAttackStatusLens.set(Regular)(baseBarbarian)
    Creature.creatureDefenseStatusLens.set(Regular)(resetAttackStatus).asInstanceOf[BaseBarbarian]
  }

  val inRageLens: Lens[BaseBarbarian, Boolean] = Lens[BaseBarbarian, Boolean](_.inRage) { rage =>
    {
      case b: Barbarian    => Barbarian._inRage.set(rage)(b)
      case b: Berserker    => Berserker._inRage.set(rage)(b)
    }
  }

  val rageUsagesLens: Lens[BaseBarbarian, Int] = Lens[BaseBarbarian, Int](_.rageUsages) { rageNum =>
    {
      case b: Barbarian    => Barbarian._rageUsages.set(rageNum)(b)
      case b: Berserker    => Berserker._rageUsages.set(rageNum)(b)
    }
  }

  val rageTurnsLeftLens: Lens[BaseBarbarian, Int] = Lens[BaseBarbarian, Int](_.rageTurnsLeft) {
    turnsLeft =>
      {
        case b: Barbarian    => Barbarian._rageTurnsLeft.set(turnsLeft)(b)
        case b: Berserker    => Berserker._rageTurnsLeft.set(turnsLeft)(b)
      }
  }
}
