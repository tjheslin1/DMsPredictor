package io.github.tjheslin1.dmspredictor.classes.barbarian

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities.nextAbilityToUseInConjunction
import io.github.tjheslin1.dmspredictor.classes.Player.playerBonusActionUsedLens
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian._
import io.github.tjheslin1.dmspredictor.model.Creature.creatureResistancesLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._

object BaseBarbarianAbilities {

  def rage(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val barbarian = combatant.creature.asInstanceOf[BaseBarbarian]

    val name                         = "Rage"
    val order                        = currentOrder
    val abilityAction: AbilityAction = BonusAction
    val levelRequirement: Level      = LevelOne

    val triggerMet: Boolean   = barbarian.inRage == false
    def conditionMet: Boolean = barbarian.rageUsages > 0

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val ragingBarbarianCombatant = Combatant.creatureLens.set(updateRagingBarbarian(barbarian))(combatant)

      nextAbilityToUseInConjunction(ragingBarbarianCombatant, order, AbilityAction.action).fold{
        Actions.attackAndDamage()
      }
    }

    def update: Creature = barbarian

    private def updateRagingBarbarian(unragedBarbarian: BaseBarbarian): Creature = {
      val updatedRageUsages = unragedBarbarian.rageUsages - 1

      val updatedBarbarian       = rageUsagesLens.set(updatedRageUsages)(unragedBarbarian)
      val rageTurnsLeftBarbarian = rageTurnsLeftLens.set(10)(updatedBarbarian)

      val resistantBarbarian = creatureResistancesLens
        .set(List(Bludgeoning, Piercing, Slashing))(rageTurnsLeftBarbarian)
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
    val levelRequirement: Level      = LevelOne

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = true

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = (combatant, none[Combatant])

    def update: Creature = {
      val recklessBarbarian = Creature.creatureAttackStatusLens.set(Advantage)(barbarian)

      Creature.creatureDefenseStatusLens.set(Disadvantage)(recklessBarbarian)
    }
  }
}
