package io.github.tjheslin1.dmspredictor.classes.cleric

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.condition.Turned
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifierForSchool
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters

object BaseClericAbilities extends LazyLogging {

  def turnUndead(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

    val name: String                 = "Turn Undead"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelTwo
    val abilityAction: AbilityAction = WholeAction

    def conditionMet: Boolean = baseCleric.channelDivinityUsed == false
    def triggerMet(others: List[Combatant]): Boolean =
      monsters(others).exists(_.creature.creatureType == Undead)

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${baseCleric.name} used $name")

      monsters(others).filter(_.creature.creatureType == Undead) match {
        case List() => (combatant, List.empty[Combatant])
        case undeadTargets: List[Combatant] =>
          val dc = 8 + baseCleric.proficiencyBonus + attributeModifierForSchool(baseCleric)

          val updatedUndead = undeadTargets.map { undead =>
            if (savingThrowPassed(dc, Wisdom, undead.creature)) undead
            else
              (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
                .set(undead.creature.conditions ++ List(Turned(dc, 10)))(undead)
          }

          (combatant, updatedUndead)
      }
    }

    def update: Creature = BaseCleric.channelDivinityUsedLens.set(true)(baseCleric)
  }
}
