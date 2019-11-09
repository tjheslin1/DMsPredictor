package io.github.tjheslin1.dmspredictor.monsters

import cats.data.NonEmptyList
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.ListOps._

object MonsterAbilities extends LazyLogging {

  def multiAttack(currentOrder: Int, numberOfAttacks: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val monster = combatant.creature.asInstanceOf[Monster]

      val name: String = s"Multi attack (Monster): $numberOfAttacks attacks"
      val order: Int   = currentOrder

      val levelRequirement: Level = LevelOne

      val abilityAction: AbilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = true
      def conditionMet: Boolean                        = true

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${monster.name} used $name")

        (1 to numberOfAttacks).foldLeft((combatant, others)) {
          case ((attacker, otherTargets), _) =>
            nextAbilityToUseInConjunction(
              attacker,
              otherTargets,
              order,
              NonEmptyList.of(SingleAttack)
            ).fold {
              nextToFocus(attacker, players(otherTargets), focus).fold((attacker, otherTargets)) {
                target =>
                  val (updatedMonster, updatedEnemy, updatedOthers) =
                    attackAndDamage(attacker, target, otherTargets.except(target))

                  (updatedMonster, updatedOthers.replace(updatedEnemy))
              }
            } { ability =>
              useAdditionalAbility(ability, attacker, otherTargets, focus)
            }
        }
      }

      def update: Creature = monster
    }
}
