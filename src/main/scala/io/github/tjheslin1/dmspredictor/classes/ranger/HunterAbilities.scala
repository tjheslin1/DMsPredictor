package io.github.tjheslin1.dmspredictor.classes.ranger

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus

object HunterAbilities extends LazyLogging {

  def ColossusSlayer(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val name: String                 = "Colossus Slayer"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelThree
    val abilityAction: AbilityAction = OnWeaponDamage

    def triggerMet(others: List[Combatant], focus: Focus): Boolean =
      ??? // nextToFocus(combatant, others, )

    def conditionMet: Boolean = ???

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = ???

    def update: Creature = ???
  }
}
