package io.github.tjheslin1.dmspredictor.monsters.lich

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, resolveDamage}
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, SingleAttack}
import io.github.tjheslin1.dmspredictor.model.condition.Condition.addCondition
import io.github.tjheslin1.dmspredictor.model.condition.Paralyzed
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich.ParalyzingTouch
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich.ParalyzingTouch.ParalyzingSaveDC
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.ListOps._

object LichAbilities extends LazyLogging {

  def paralyzingTouch(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val lich = combatant.creature.asInstanceOf[Lich]

    val name: String = "Paralyzing Touch (Lich)"
    val order: Int   = currentOrder

    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = SingleAttack

    def triggerMet(others: List[Combatant]): Boolean = true
    def conditionMet: Boolean                        = true

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"Lich used $name")

      nextToFocus(combatant, players(others), focus) match {
        case None => (combatant, others)
        case Some(target) =>
          val (attackResult, hitTarget) = attack(combatant, ParalyzingTouch, target)

          val (updatedLich, updatedTarget, updatedOthers) =
            resolveDamage(combatant, hitTarget, others, ParalyzingTouch, attackResult)

          val (passedSave, updatedSavingThrowCreature) =
            savingThrowPassed(ParalyzingSaveDC, Constitution, updatedTarget.creature)

          val updatedSavingThrowTarget =
            Combatant.creatureLens.set(updatedSavingThrowCreature)(updatedTarget)

          val conditionUpdatedTarget = attackResult match {
            case CriticalHit | Hit if passedSave == false =>
              val conditionUpdatedCreature =
                addCondition(updatedSavingThrowTarget.creature, Paralyzed(ParalyzingSaveDC, 10, Constitution))

              Combatant.creatureLens.set(conditionUpdatedCreature)(updatedSavingThrowTarget)
            case _ =>
              updatedSavingThrowTarget
          }

          (updatedLich, updatedOthers.replace(conditionUpdatedTarget))
      }
    }

    def update: Creature = lich
  }
}
