package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, resolveDamage}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import io.github.tjheslin1.dmspredictor.util.IntOps._

object FighterAbilities {

  import Fighter._

  def secondWind(combatant: Combatant): Ability[Fighter] = new Ability[Fighter](combatant) {
    val fighter = combatant.creature.asInstanceOf[Fighter]

    val levelRequirement = LevelTwo
    val triggerMet       = fighter.health <= fighter.maxHealth / 2
    val conditionMet     = fighter.level.value >= levelRequirement && fighter.secondWindUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      (combatant.copy(
         creature =
           fighter.copy(health = Math.min(fighter.maxHealth, fighter.health + (1 * HitDice) + fighter.level.value))),
       None)

    def update: Fighter = fighter.copy(secondWindUsed = true)
  }

  def twoWeaponFighting(combatant: Combatant): Ability[Fighter] = new Ability[Fighter](combatant) {
    val fighter = combatant.creature.asInstanceOf[Fighter]

    val levelRequirement: Level = LevelOne
    val triggerMet: Boolean     = true
    val conditionMet: Boolean = fighter.offHand match {
      case Some(w: Weapon) =>
        w.twoHanded == false && fighter.baseWeapon.twoHanded == false && fighter.fightingStyles.contains(
          TwoWeaponFighting)
      case _ => false
    }

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      target match {
        case Some(target: Combatant) =>
          val mainHandAttack = attack(combatant, fighter.weapon, target)

          val (attacker1, attackTarget1) =
            if (mainHandAttack.result > 0) resolveDamage(combatant, target, mainHandAttack)
            else
              (combatant, target)

          val offHandAttack = attack(attacker1, fighter.offHand.get.asInstanceOf[Weapon], attackTarget1)

          val (attacker2, attackTarget2) =
            if (offHandAttack.result > 0) resolveDamage(attacker1, attackTarget1, offHandAttack)
            else
              (attacker1, attackTarget1)

          (attacker2, attackTarget2.some)
        case None => (combatant, None)
      }
    }
    def update: Fighter = fighter
  }

}
