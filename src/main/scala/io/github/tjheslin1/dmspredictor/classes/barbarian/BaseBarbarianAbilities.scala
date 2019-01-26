package io.github.tjheslin1.dmspredictor.classes.barbarian

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

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = ???

    def update: Creature = {
      val updatedRageUsages = barbarian.rageUsages - 1
      BaseBarbarian.rageUsagesLens.set(updatedRageUsages)(barbarian)
    }
  }
}
