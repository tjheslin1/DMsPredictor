package io.github.tjheslin1.dmspredictor.classes

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import io.github.tjheslin1.dmspredictor.strategy.Ability.SingleAttack

object CoreAbilities {

  val ExtraAttack = "Extra Attack"

  val standardCoreAbilities: List[CombatantAbility] = List(
    extraAttack(1)
  )

  def extraAttack(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val player = combatant.creature.asInstanceOf[Player]

    val name  = ExtraAttack
    val order = currentOrder
    val levelRequirement: Level = LevelFive
    val abilityAction = SingleAttack

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = player.level >= levelRequirement

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          nextAbilityToUseInConjunction(combatant, order)
            .fold {
              val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)
              (updatedAttacker, updatedTarget.some)
            } { nextAbility =>
              println(s">>> Core - nextAbility: ${nextAbility(combatant).name}")
              useAdditionalAbility(nextAbility, combatant, target)
            }
      }

    def update: Creature = combatant.creature
  }
}
