package io.github.tjheslin1.dmspredictor.monsters

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case class Werewolf(creature: Creature)

object Werewolf {

  def calculateHealth[_: RS] = (9 * D8) + 18

  val hydbridFormClaw = new Weapon {
    val name: String = "hybrid form claw"
    val damageType   = Slashing

    def damage(implicit rollStrategy: RollStrategy): Int = (2 * D4) + 2
  }

  def apply[_: RS](): Werewolf =
    Werewolf(Creature(calculateHealth, BaseStats(15, 13, 14, 10, 11, 10), 12, hydbridFormClaw, Monster))
}
