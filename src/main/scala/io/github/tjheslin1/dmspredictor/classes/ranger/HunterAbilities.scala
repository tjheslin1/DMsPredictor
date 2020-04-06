package io.github.tjheslin1.dmspredictor.classes.ranger

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus

object HunterAbilities extends LazyLogging {

  def ColossusSlayer(currentOrder: Int)(combatant: Combatant): Ability = new OnWeaponDamageAbility(combatant) {
    val name: String                 = "Colossus Slayer"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelThree

    def triggerMet(others: List[Combatant]): Boolean =
      ??? // nextToFocus(combatant, others, )

    def conditionMet: Boolean = ???

    def damage[_: RS](): Int = ???
  }
}
