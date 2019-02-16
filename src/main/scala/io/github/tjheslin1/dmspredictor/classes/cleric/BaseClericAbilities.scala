package io.github.tjheslin1.dmspredictor.classes.cleric
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.{Combatant, Creature, Level, LevelTwo, RS}

object BaseClericAbilities {

  def castSpell(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {

    val name: String                 = "Turn Undead"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelTwo
    val abilityAction: AbilityAction = WholeAction

    def triggerMet(target: Option[Combatant]): Boolean                                                          = ???
    def conditionMet: Boolean                                                        = ???
    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = ???
    def update: Creature                                                             = ???
  }
}
