package io.github.tjheslin1.dmspredictor.classes.cleric

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifier
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.SacredFlame

object BaseClericAbilities extends LazyLogging {

  def sacredFlame(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

    val name: String                 = "Sacred Flame"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = WholeAction

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = true

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${baseCleric.name} cast $name")

      target match {
        case None => (combatant, none[Combatant])
        case Some(cantripTarget) =>
          if (spellSavingThrowPassed(SacredFlame, Dexterity, cantripTarget.creature))
            (combatant, cantripTarget.some)
          else {
            val dmg = SacredFlame.damage(baseCleric.level)

            val adjustedDmg = Actions.adjustedDamage(dmg, SacredFlame.damageType, cantripTarget)

            val damagedTarget =
              cantripTarget.copy(
                creature = cantripTarget.creature.updateHealth(Math.negateExact(adjustedDmg)))

            (combatant, damagedTarget.some)
          }
      }
    }

    def update: Creature = baseCleric

    private def spellSavingThrowPassed[_: RS](spell: Spell,
                                              attribute: Attribute,
                                              target: Creature): Boolean =
      if ((D20.roll() + attributeModifier(target, attribute)) >= spell.spellSaveDc(baseCleric))
        true
      else false
  }
}
