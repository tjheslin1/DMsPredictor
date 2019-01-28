package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Werewolf.HydbridFormClaw
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Werewolf(health: Int,
                                 maxHealth: Int,
                                 stats: BaseStats,
                                 armourClass: Int,
                                 baseWeapon: Weapon = HydbridFormClaw,
                                 armour: Armour = NoArmour,
                                 offHand: Option[Equipment] = None,
                                 resistances: List[DamageType] = List(),
                                 immunities: List[DamageType] = List(),
                                 abilities: List[CombatantAbility] = List.empty,
                                 attackStatus: AttackStatus = Regular,
                                 defenseStatus: AttackStatus = Regular,
                                 name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType         = Monster
  val proficiencyBonus: ProficiencyBonus = 0

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def turnReset(): Creature = this
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
    s"Werewolf: " +
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

  val strengthLens: Lens[Werewolf, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Werewolf, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Werewolf, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Werewolf, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Werewolf, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Werewolf, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
