package io.github.tjheslin1.dmspredictor.classes.rogue

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters

object BaseRogueAbilities extends LazyLogging {

  def hide(currentPriority: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseRogue = combatant.creature.asInstanceOf[BaseRogue]

    val name: String = "Hide (Rogue)"
    val order: Int   = currentPriority

    val levelRequirement: Level = LevelTwo

    val abilityAction: AbilityAction = BonusAction

    def triggerMet(others: List[Combatant]): Boolean = true

    def conditionMet: Boolean = baseRogue.bonusActionUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      val hideDc = if (baseRogue.stealthProficiency) {
        D20.roll() + mod(baseRogue.stats.dexterity) + baseRogue.proficiencyBonus
      } else {
        D20.roll() + mod(baseRogue.stats.dexterity)
      }

      val updatedRogue = monsters(others).foldLeft(baseRogue) {
        case (hidingRogue, enemy) =>
          if (savingThrowPassed(hideDc, Wisdom, enemy.creature)) hidingRogue
          else {
            val updatedEnemiesHiddenFrom = hidingRogue.hiddenFrom ++ List(enemy)

            BaseRogue.hiddenFromLens.set(updatedEnemiesHiddenFrom)(baseRogue)
          }
      }

      val updatedCombatant = Combatant.creatureLens.set(updatedRogue)(combatant)

      (updatedCombatant, others)
    }

    def update: Creature = Player.playerBonusActionUsedLens.set(true)(baseRogue)
  }
}
