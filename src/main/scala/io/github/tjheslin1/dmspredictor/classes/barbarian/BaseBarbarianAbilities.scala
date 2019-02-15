package io.github.tjheslin1.dmspredictor.classes.barbarian

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.Player.playerBonusActionUsedLens
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model.Creature.creatureResistancesLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._

object BaseBarbarianAbilities extends LazyLogging {

  def rage(currentOrder: Int,
           rageResistances: List[DamageType] = List(Bludgeoning, Piercing, Slashing))(
      combatant: Combatant): Ability = new Ability(combatant) {
    val barbarian = combatant.creature.asInstanceOf[BaseBarbarian]

    val name                         = "Rage"
    val order                        = currentOrder
    val abilityAction: AbilityAction = BonusAction
    val levelRequirement: Level      = LevelOne

    val triggerMet: Boolean   = barbarian.inRage == false
    def conditionMet: Boolean = barbarian.rageUsages > 0

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used Rage")

      val ragingBarbarianCombatant =
        Combatant.creatureLens.set(updateRagingBarbarian(barbarian))(combatant)

      target match {
        case None => (ragingBarbarianCombatant, none[Combatant])
        case Some(targetOfAttack) =>
          nextAbilityToUseInConjunction(ragingBarbarianCombatant, order, AbilityAction.Action)
            .fold {
              val (updatedAttacker, updatedTarget) =
                attackAndDamage(ragingBarbarianCombatant, targetOfAttack)
              (updatedAttacker, updatedTarget.some)
            }(nextAbility =>
              useAdditionalAbility(nextAbility, ragingBarbarianCombatant, targetOfAttack))
      }
    }

    def update: Creature = barbarian

    private def updateRagingBarbarian(unragedBarbarian: BaseBarbarian): Creature = {
      val updatedRageUsages = unragedBarbarian.rageUsages - 1

      val updatedBarbarian       = rageUsagesLens.set(updatedRageUsages)(unragedBarbarian)
      val rageTurnsLeftBarbarian = rageTurnsLeftLens.set(10)(updatedBarbarian)

      val resistantBarbarian = creatureResistancesLens
        .set(barbarian.resistances ++ rageResistances)(rageTurnsLeftBarbarian)
        .asInstanceOf[BaseBarbarian]

      val bonusActionUsedBarbarian =
        playerBonusActionUsedLens.set(true)(resistantBarbarian).asInstanceOf[BaseBarbarian]

      inRageLens.set(true)(bonusActionUsedBarbarian)
    }
  }

  def recklessAttack(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val barbarian = combatant.creature.asInstanceOf[BaseBarbarian]

    val name                         = "Reckless Attack"
    val order                        = currentOrder
    val abilityAction: AbilityAction = SingleAttack
    val levelRequirement: Level      = LevelTwo

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = barbarian.level >= levelRequirement

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${combatant.creature.name} is recklessly attacking")

      val recklessBarbarianCombatant =
        Combatant.creatureLens.set(updateRecklessBarbarian(barbarian))(combatant)

      target match {
        case None => (recklessBarbarianCombatant, none[Combatant])
        case Some(targetOfAttack) =>
          val (updatedAttacker, updatedTarget) =
            attackAndDamage(recklessBarbarianCombatant, targetOfAttack)

          (updatedAttacker, updatedTarget.some)
      }
    }

    def update: Creature = barbarian

    private def updateRecklessBarbarian(calmBarbarian: BaseBarbarian): Creature = {
      val advantageBarbarian = Creature.creatureAttackStatusLens.set(Advantage)(calmBarbarian)

      Creature.creatureDefenseStatusLens.set(Disadvantage)(advantageBarbarian)
    }
  }
}
