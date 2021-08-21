package io.github.tjheslin1.dmspredictor.classes.cleric

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.condition.Condition.addCondition
import io.github.tjheslin1.dmspredictor.model.condition.{Turned, TurnedCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.ListOps._

object BaseClericAbilities extends LazyLogging {

  def turnUndead(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

      val name: String                 = "Turn Undead"
      val order: Int                   = currentOrder
      val levelRequirement: Level      = LevelTwo
      val abilityAction: AbilityAction = WholeAction

      def conditionMet: Boolean =
        baseCleric.level >= levelRequirement && baseCleric.channelDivinityUsed == false

      def triggerMet(others: List[Combatant]): Boolean = monsters(others).exists(
        _.creature.creatureType == Undead)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${baseCleric.name} used $name")

        monsters(others).filter(_.creature.creatureType == Undead) match {
          case List() =>
            (combatant, others)
          case undeadTargets: List[Combatant] =>
            val dc = spellSaveDc(baseCleric)

            val updatedUndead = undeadTargets.map { undead =>
              if (undead.creature.conditionImmunities.contains(TurnedCondition))
                undead
              else if (undead.creature.conditionResistances.contains(TurnedCondition)) {
                val (passed, updatedCreature) = savingThrowWithAdvantagePassed(
                  dc,
                  Wisdom,
                  undead.creature)

                val updatedUndead = Combatant.creatureLens.set(updatedCreature)(undead)

                if (passed)
                  updatedUndead
                else
                  addCondition(updatedUndead, Turned(dc, 10))
              } else {
                val (passed, updatedCreature) = savingThrowPassed(dc, Wisdom, undead.creature)

                val updatedUndead = Combatant.creatureLens.set(updatedCreature)(undead)

                if (passed)
                  updatedUndead
                else
                  addCondition(updatedUndead, Turned(dc, 10))
              }
            }

            (combatant, others.replace(updatedUndead))
        }
      }

      def update: Creature = BaseCleric.channelDivinityUsedLens.set(true)(baseCleric)
    }

  def destroyUndead(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

      val name: String                 = "Destroy Undead"
      val order: Int                   = currentOrder
      val levelRequirement: Level      = LevelFive
      val abilityAction: AbilityAction = WholeAction

      def conditionMet: Boolean =
        baseCleric.level >= levelRequirement && baseCleric.channelDivinityUsed == false

      def triggerMet(others: List[Combatant]): Boolean = monsters(others).exists(
        _.creature.creatureType == Undead)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${baseCleric.name} used $name")

        monsters(others).filter(_.creature.creatureType == Undead) match {
          case List() =>
            (combatant, others)
          case undeadTargets: List[Combatant] =>
            val dc = spellSaveDc(baseCleric)

            val updatedUndead = undeadTargets.map { undead =>
              if (undead.creature.conditionImmunities.contains(TurnedCondition))
                undead
              else if (undead.creature.conditionResistances.contains(TurnedCondition)) {
                val (passed, updatedCreature) = savingThrowWithAdvantagePassed(
                  dc,
                  Wisdom,
                  undead.creature)

                val updatedUndead = Combatant.creatureLens.set(updatedCreature)(undead)

                if (passed)
                  updatedUndead
                else if (updatedCreature.asInstanceOf[Monster].challengeRating <= 0.5) {
                  logger.debug(s"${updatedCreature.name} has been Destroyed")

                  val zeroHpUndead =
                    (Combatant.creatureLens composeLens Creature.creatureHealthLens)
                      .set(0)(updatedUndead)

                  (Combatant.creatureLens composeLens Creature.creatureIsAliveLens)
                    .set(false)(zeroHpUndead)
                } else
                  (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
                    .set(updatedCreature.conditions ++ List(Turned(dc, 10)))(updatedUndead)
              } else {
                val (passed, updatedCreature) = savingThrowPassed(dc, Wisdom, undead.creature)

                val updatedUndead = Combatant.creatureLens.set(updatedCreature)(undead)

                if (passed)
                  updatedUndead
                else if (updatedCreature.asInstanceOf[Monster].challengeRating <= 0.5) {
                  logger.debug(s"${updatedCreature.name} has been Destroyed")

                  val zeroHpUndead =
                    (Combatant.creatureLens composeLens Creature.creatureHealthLens)
                      .set(0)(updatedUndead)

                  (Combatant.creatureLens composeLens Creature.creatureIsAliveLens)
                    .set(false)(zeroHpUndead)
                } else {
                  logger.debug(s"${updatedCreature.name} has been Turned")

                  (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
                    .set(undead.creature.conditions ++ List(Turned(dc, 10)))(updatedUndead)
                }
              }
            }

            (combatant, others.replace(updatedUndead))
        }
      }

      def update: Creature = BaseCleric.channelDivinityUsedLens.set(true)(baseCleric)
    }
}
