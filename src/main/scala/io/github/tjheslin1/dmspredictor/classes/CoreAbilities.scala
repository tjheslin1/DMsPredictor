package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions.{attackAndDamage, attackAndDamageTimes}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CoreAbilities extends LazyLogging {

  val ExtraAttack = "Extra Attack"

  val standardCoreAbilities: List[CombatantAbility] = List(
    extraAttack(1)
  )

  def extraAttack(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val player = combatant.creature.asInstanceOf[Player]

      val name             = ExtraAttack
      val order            = currentOrder
      val levelRequirement = LevelFive
      val abilityAction    = SingleAttack

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = player.level >= levelRequirement

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        nextToFocus(combatant, monsters(others), focus) match {
          case None =>
            (combatant, others)
          case Some(target) =>
            nextAbilityToUseInConjunction(combatant, others, order, NonEmptyList.of(SingleAttack))
              .fold {
                val (updatedAttacker, updatedTarget, updatedOthers) = attackAndDamageTimes(
                  2,
                  combatant,
                  target,
                  others)

                (updatedAttacker, updatedOthers.replace(updatedTarget))
              } { nextAbility =>
                val (updatedCombatant, updatedOthers) = useAdditionalAbility(
                  nextAbility,
                  combatant,
                  others,
                  focus)

                nextAbilityToUseInConjunction(
                  updatedCombatant,
                  updatedOthers,
                  order,
                  NonEmptyList.one(SingleAttack)
                ).fold {
                  nextToFocus(updatedCombatant, monsters(updatedOthers), focus).fold {
                    (updatedCombatant, updatedOthers)
                  } { focusTarget =>
                    val (updatedAttacker, updatedAttackedTarget, updatedOthers2) = attackAndDamage(
                      updatedCombatant,
                      focusTarget,
                      updatedOthers)

                    (updatedAttacker, updatedOthers2.replace(updatedAttackedTarget))
                  }
                } { nextAbility2 =>
                  useAdditionalAbility(nextAbility2, updatedCombatant, updatedOthers, focus)
                }
              }
        }
      }

      def update: Creature = player
    }
}
