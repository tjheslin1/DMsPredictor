package io.github.tjheslin1.dmspredictor.classes.rogue

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.Armour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import monocle.Lens

trait BaseRogue extends Player with Product with Serializable {

  val hiddenFrom: List[Combatant]

  def resetStartOfTurn(): Creature       = this
  def scoresCritical(roll: Int): Boolean = roll == 20
}

object BaseRogue {

  val HitDice         = D8
  val SneakAttackDice = D6

  val sneakAttackDamage: Map[Level, Int] = Map(
    LevelOne   -> 1,
    LevelTwo   -> 1,
    LevelThree -> 2,
    LevelFour  -> 2,
    LevelFive  -> 3
  )

  def calculateHealth(level: Level, constitutionScore: Stat): Int = Player.calculateHealth(
    HitDice,
    level,
    constitutionScore)

  def calculateArmourClass(stats: BaseStats, armour: Armour, offHand: Option[Equipment]): Int =
    armour.armourClass(stats.dexterity)

  val hiddenFromLens: Lens[BaseRogue, List[Combatant]] =
    Lens[BaseRogue, List[Combatant]](_.hiddenFrom) { enemiesHiddenFrom =>
      {
        case r: Rogue => Rogue._hiddenFrom.set(enemiesHiddenFrom)(r)

        case _ => throw new NotImplementedError("Missing a case in hiddenFromLens")
      }
    }
}
