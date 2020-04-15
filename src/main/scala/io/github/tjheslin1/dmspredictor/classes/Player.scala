package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.classes.ranger._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait Player extends Creature {

  val level: Level
  val bonusActionUsed: Boolean
  val proficiencyBonus: ProficiencyBonus
  val savingThrowProficiencies: NonEmptyList[Attribute]

  val creatureType: CreatureType = PlayerCharacter
}

object Player {

  def calculateHealth(hitDice: Dice, level: Level, constitutionScore: Stat): Int =
    (hitDice.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(hitDice) + mod(
      constitutionScore
    )))

  val playerBonusActionUsedLens: Lens[Player, Boolean] = Lens[Player, Boolean](_.bonusActionUsed) {
    bonusUsed =>
      {
        case c: Champion => Champion._bonusActionUsed.set(bonusUsed)(c)
        case c: Fighter  => Fighter._bonusActionUsed.set(bonusUsed)(c)

        case c: Barbarian => Barbarian._bonusActionUsed.set(bonusUsed)(c)
        case c: Berserker => Berserker._bonusActionUsed.set(bonusUsed)(c)

        case c: Cleric => Cleric._bonusActionUsed.set(bonusUsed)(c)

        case c: Rogue => Rogue._bonusActionUsed.set(bonusUsed)(c)

        case c: Wizard => Wizard._bonusActionUsed.set(bonusUsed)(c)

        case c: Ranger => Ranger._bonusActionUsed.set(bonusUsed)(c)
        case c: Hunter => Hunter._bonusActionUsed.set(bonusUsed)(c)

        case _ =>
          throw new NotImplementedError(
            "Missing playerBonusActionUsedLens lens for your new implementation of Player!"
          )
      }
  }

  val playerProficiencyBonusLens: Lens[Player, ProficiencyBonus] =
    Lens[Player, ProficiencyBonus](_.proficiencyBonus) { profBonus =>
      {
        case c: Champion => Champion._proficiencyBonus.set(profBonus)(c)
        case c: Fighter  => Fighter._proficiencyBonus.set(profBonus)(c)

        case c: Barbarian => Barbarian._proficiencyBonus.set(profBonus)(c)
        case c: Berserker => Berserker._proficiencyBonus.set(profBonus)(c)

        case c: Cleric => Cleric._proficiencyBonus.set(profBonus)(c)

        case c: Rogue => Rogue._proficiencyBonus.set(profBonus)(c)

        case c: Wizard => Wizard._proficiencyBonus.set(profBonus)(c)

        case c: Ranger => Ranger._proficiencyBonus.set(profBonus)(c)
        case c: Hunter => Hunter._proficiencyBonus.set(profBonus)(c)

        case _ =>
          throw new NotImplementedError(
            "Missing playerProficiencyBonusLens lens for your new implementation of Player!"
          )
      }
    }
}
