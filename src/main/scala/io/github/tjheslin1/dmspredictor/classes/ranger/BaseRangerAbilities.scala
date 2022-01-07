package io.github.tjheslin1.dmspredictor.classes.ranger

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.ListOps._

object BaseRangerAbilities extends LazyLogging {

  def twoWeaponFighting(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val baseRanger = combatant.creature.asInstanceOf[BaseRanger]

      val name             = "Two Weapon Fighting"
      val order            = currentOrder
      val levelRequirement = LevelTwo
      val abilityAction    = SingleAttack

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean =
        combatant.creature.offHand match {
          case Some(w: Weapon) =>
            baseRanger.level >= levelRequirement &&
            baseRanger.bonusActionUsed == false &&
            w.twoHanded == false &&
            combatant.creature.baseWeapon.twoHanded == false
          case _ => false
        }

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used two weapon fighting")

        nextToFocus(combatant, monsters(others), focus) match {
          case None => (combatant, others)
          case Some(attackTarget) =>
            val (mainHandAttack, hitTarget) = attack(
              combatant,
              combatant.creature.weapon,
              attackTarget)

            val (updatedAttacker, attackTarget1, updatedOthers) =
              if (mainHandAttack.result > 0)
                resolveDamageMainHand(combatant, hitTarget, others, mainHandAttack)
              else
                (combatant, hitTarget, others)

            val updatedEnemies = monsters(updatedOthers).replace(attackTarget1)

            nextToFocus(combatant, updatedEnemies, focus) match {
              case None => (combatant, others)
              case Some(nextTarget) =>
                val offHandWeapon = combatant.creature.offHand.get.asInstanceOf[Weapon]
                val (offHandAttack, nextHitTarget) = attack(
                  updatedAttacker,
                  offHandWeapon,
                  nextTarget)

                val offHandStatModifier = baseRanger.fightingStyles.contains(TwoWeaponFighting)

                val (attacker2, attackTarget2, updatedOthers2) =
                  if (offHandAttack.result > 0)
                    resolveDamage(
                      updatedAttacker,
                      nextHitTarget,
                      updatedOthers,
                      offHandWeapon,
                      offHandAttack,
                      addStatModifier = offHandStatModifier
                    )
                  else
                    (updatedAttacker, nextHitTarget, updatedOthers)

                (attacker2, updatedOthers2.replace(attackTarget2))
            }
        }
      }

      def update: Creature = Player.playerBonusActionUsedLens.set(true)(baseRanger)
    }
}
