package io.github.tjheslin1.dmspredictor.monsters.vampire

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, resolveDamage}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, SingleAttack}
import io.github.tjheslin1.dmspredictor.model.condition.Grappled
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.IntOps._

object VampireAbilities extends LazyLogging {

  def bite(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val vampire = combatant.creature.asInstanceOf[Vampire]

    val name: String = "Bite (Vampire)"
    val order: Int   = currentOrder

    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = SingleAttack

    def triggerMet(others: List[Combatant]): Boolean =
      others.exists(_.creature.conditions.map(_.name).contains(Grappled.name))

    def conditionMet: Boolean = vampire.biteUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"Vampire used $name")

      val grappledEnemies =
        players(others).filter(_.creature.conditions.map(_.name).contains(Grappled.name))

      nextToFocus(grappledEnemies, focus) match {
        case None => (combatant, List.empty[Combatant])
        case Some(grappledTarget) =>
          val attackResult = attack(combatant, Bite, grappledTarget)

          val (updatedVampireCombatant, updatedTarget) =
            resolveDamage(combatant, grappledTarget, Bite, attackResult)

          val necroticDamage = attackResult match {
            case CriticalHit  => Bite.necroticDamage + Bite.necroticDamage
            case Hit          => Bite.necroticDamage
            case Miss         => 0
            case CriticalMiss => 0
          }

          val updatedVampire = updatedVampireCombatant.creature.asInstanceOf[Vampire]

          val restoredVampire = Combatant.creatureLens.set(
            updatedVampire.restoreHealth(necroticDamage))(updatedVampireCombatant)

          val updatedHealth    = updatedTarget.creature.health - necroticDamage
          val updatedMaxHealth = updatedTarget.creature.maxHealth - necroticDamage

          val updatedHealthCleric = (Combatant.creatureLens composeLens Creature.creatureHealthLens)
            .set(updatedHealth)(updatedTarget)
          val updatedMaxHealthCleric =
            (Combatant.creatureLens composeLens Creature.creatureMaxHealthLens)
              .set(updatedMaxHealth)(updatedHealthCleric)

          (restoredVampire, List(updatedMaxHealthCleric))
      }
    }

    def update: Creature = vampire.copy(biteUsed = true)
  }

  case object Bite extends Weapon {
    val name: String           = "Bite (Vampire)"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Piercing
    val twoHanded: Boolean     = true

    override val hitBonus: Int = 9

    def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6 // +4 from Strength

    def necroticDamage[_: RS] = 3 * D6
  }
}

