package io.github.tjheslin1.dmspredictor.classes.ranger

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object HunterAbilities extends LazyLogging {

  def colossusSlayer(currentOrder: Int)(combatant: Combatant): OnWeaponDamageAbility =
    new OnWeaponDamageAbility(combatant) {
      val hunter = combatant.creature.asInstanceOf[Hunter]

      val name: String            = "Colossus Slayer"
      val order: Int              = currentOrder
      val levelRequirement: Level = LevelThree

      def triggerMet(others: List[Combatant]): Boolean =
        others match {
          case List(target) => target.creature.health < target.creature.maxHealth
          case _ =>
            throw new IllegalArgumentException(
              s"Expected one combatant passed to triggerMet but found ${others.size}"
            )
        }

      def conditionMet: Boolean = hunter.colossusSlayerUsed == false

      def damage[_: RS](): Int = 1 * D8

      def update: Creature = Hunter._colossusSlayerUsed.set(true)(hunter)
    }
}
