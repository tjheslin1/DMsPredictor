package io.github.tjheslin1.dmspredictor.classes.cleric

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifierForSchool

object BaseClericAbilities extends LazyLogging {

  def turnUndead(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

    val name: String                 = "Turn Undead"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelTwo
    val abilityAction: AbilityAction = WholeAction

    def conditionMet: Boolean = baseCleric.channelDivinityUsed == false
    def triggerMet(target: Option[Combatant]): Boolean = target match {
      case None                                                               => false
      case Some(targetOfTurn) if targetOfTurn.creature.creatureType == Undead => true
      case _                                                                  => false
    }

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${baseCleric.name} used $name")

      target match {
        case None => (combatant, none[Combatant])
        case Some(targetOfTurn) =>
          val dc = 8 + baseCleric.proficiencyBonus + attributeModifierForSchool(baseCleric)

          if (savingThrowPassed(dc, Wisdom, targetOfTurn.creature))
            (combatant, none[Combatant])
          else {
            val updatedTarget = (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
              .set(targetOfTurn.creature.conditions ++ List(Turned(dc, Wisdom, 10)))(targetOfTurn)

            (combatant, updatedTarget.some)
          }
      }
    }

    def update: Creature = BaseCleric.channelDivinityUsedLens.set(true)(baseCleric)
  }
}
