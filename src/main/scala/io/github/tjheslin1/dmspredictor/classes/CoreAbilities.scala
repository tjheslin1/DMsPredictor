package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.data.NonEmptyList.one
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._

object CoreAbilities {

  val ExtraAttack = "Extra Attack"

  val standardCoreAbilities: List[CombatantAbility] = List(
    extraAttack(1)
  )

  def extraAttack(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val player = combatant.creature.asInstanceOf[Player]

    val name             = ExtraAttack
    val order            = currentOrder
    val levelRequirement = LevelFive
    val abilityAction    = SingleAttack

    val triggerMet: Boolean   = true
    def conditionMet: Boolean = player.level >= levelRequirement && player.bonusActionUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          nextAbilityToUseInConjunction(combatant, order, NonEmptyList.of(BonusAction, SingleAttack))
            .fold {
              val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)
              (updatedAttacker, updatedTarget.some)
            } { nextAbility =>
              val (updatedCombatant, updatedTargetOfAbility) = useAdditionalAbility(nextAbility, combatant, target)

              updatedTargetOfAbility.fold((updatedCombatant, none[Combatant])) { updatedTarget =>
                nextAbilityToUseInConjunction(updatedCombatant, order, one(SingleAttack)).fold {
                  val (updatedAttacker, updatedAttackedTarget) =
                    attackAndDamageTimes(1, updatedCombatant, updatedTarget)
                  (updatedAttacker, updatedAttackedTarget.some)
                } { nextAbility2 =>
                  useAdditionalAbility(nextAbility2, updatedCombatant, updatedTarget)
                }
              }
            }
      }

    def update: Creature = combatant.creature
  }
}
