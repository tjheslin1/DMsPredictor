package io.github.tjheslin1.dmspredictor.classes

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability

object CoreAbilities {

  val ExtraAttack = "Extra Attack"

  val standardCoreAbilities: List[CreatureAbility] = List(
    1 -> extraAttack
  )

  def extraAttack(combatant: Combatant): Ability = new Ability(combatant) {
    val player = combatant.creature.asInstanceOf[Player]

    val name                    = ExtraAttack
    val levelRequirement: Level = LevelFive

    def triggerMet: Boolean   = true
    def conditionMet: Boolean = player.level >= levelRequirement

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)
          (updatedAttacker, updatedTarget.some)
      }

    def update: Creature = combatant.creature
  }
}
