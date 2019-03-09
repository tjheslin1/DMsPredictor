package io.github.tjheslin1.dmspredictor.classes.cleric

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellLevel
import io.github.tjheslin1.dmspredictor.strategy.Focus

object LifeClericAbilities extends LazyLogging {

  def discipleOfLifeBonusHealing(spellLevel: SpellLevel): Int = 2 + spellLevel

  def discipleOfLife(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val lifeCleric = combatant.creature.asInstanceOf[Cleric]

      val name: String = "Disciple of Life"
      val order: Int   = currentOrder

      val levelRequirement: Level      = LevelOne
      val abilityAction: AbilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = healingSpellTriggerMet(others)

      def conditionMet: Boolean = healingSpellConditionMet(lifeCleric)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${lifeCleric.name} used $name")

        if (lifeCleric.abilities
              .map(_(combatant).name)
              .contains(CastSingleTargetHealingSpellName)) {

          healingSpellInHighestSlot(lifeCleric).fold((combatant, others)) { healingSpell =>
            castSingleTargetHealingSpell(
              order,
              bonusHealing = discipleOfLifeBonusHealing(healingSpell.spellLevel))(combatant)
              .useAbility(others, focus)
          }
        } else
          (combatant, others)
      }

      def update: Creature = lifeCleric
    }
}
