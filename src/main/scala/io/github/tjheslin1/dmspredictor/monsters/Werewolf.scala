package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator

case class Werewolf(health: Int,
                    stats: BaseStats,
                    armourClass: Int,
                    weapon: Weapon,
                    override val resistances: List[DamageType] = List(),
                    override val immunities: List[DamageType] = List(),
                    override val name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = Monster

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))
}

object Werewolf {

  def calculateHealth[_: RS] = (9 * D8) + 18

  val hydbridFormClaw = new Weapon {
    val name: String = "hybrid form claw"
    val damageType   = Slashing

    def damage(implicit rollStrategy: RollStrategy): Int = (2 * D4) + 2
  }

  def apply[_: RS](): Werewolf =
    Werewolf(calculateHealth,
             BaseStats(15, 13, 14, 10, 11, 10),
             12,
             hydbridFormClaw,
             immunities = List(Bludgeoning, Piercing, Slashing))

  implicit val werewolfShow: Show[Werewolf] = Show.show { werewolf =>
    s"Fighter: " +
      s"Name: ${werewolf.name}, " +
      s"health: ${werewolf.health}, " +
      s"AC: ${werewolf.armourClass}, " +
      s"${werewolf.weapon.show}"
  }
}
