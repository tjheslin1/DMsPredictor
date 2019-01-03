package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.macros.Lenses

@Lenses("_") case class Werewolf(health: Int,
                    maxHealth: Int,
                    stats: BaseStats,
                    armourClass: Int,
                    override val resistances: List[DamageType] = List(),
                    override val immunities: List[DamageType] = List(),
                    override val name: String = NameGenerator.randomName)
    extends Creature {

  import Werewolf._

  val creatureType: CreatureType = EnemyMonster
  val proficiencyBonus: Int      = 0

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

  val baseWeapon: Weapon = HydbridFormClaw

  def weapon[_: RS]: Weapon = baseWeapon

  val abilities: List[CreatureAbility] = List.empty
  val armour: Armour                   = NoArmour
  val offHand: Option[Equipment]       = None
}

object Werewolf {

  val HitDice = D8

  def calculateHealth[_: RS] = (9 * HitDice) + 18

  def apply[_: RS](): Werewolf = {
    val hp = calculateHealth
    Werewolf(hp,
             hp,
             BaseStats(15, 13, 14, 10, 11, 10),
             armourClass = 12,
             immunities = List(Bludgeoning, Piercing, Slashing))
  }

  implicit def werewolfShow[_: RS]: Show[Werewolf] = Show.show { werewolf =>
    s"Fighter: " +
      s"Name: ${werewolf.name}, " +
      s"health: ${werewolf.health}, " +
      s"AC: ${werewolf.armourClass}"
  }

  case object HydbridFormClaw extends Weapon {

    val name       = "hybrid form claw"
    val weaponType = Melee
    val damageType = Slashing
    val twoHanded  = true

    def damage(implicit rollStrategy: RollStrategy): Int = (2 * D4) + 2

  }
}
