package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.monsters.Monster.defaultSavingThrowScores
import io.github.tjheslin1.dmspredictor.monsters.MonsterAbilities.multiAttack
import io.github.tjheslin1.dmspredictor.monsters.Werewolf._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Werewolf(
    health: Int,
    maxHealth: Int,
    stats: BaseStats = BaseStats(15, 13, 14, 10, 11, 10),
    armourClass: Int = 12,
    baseWeapon: Weapon = HydbridFormClaw,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    resistances: List[DamageType] = List.empty[DamageType],
    immunities: List[DamageType] = List(Bludgeoning, Piercing, Slashing),
    conditions: List[Condition] = List.empty,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    name: String = NameGenerator.randomName
) extends Monster {

  val challengeRating: Double                = 3.0
  val skills                                 = Skills(perception = 4, stealth = 3)
  val savingThrowScores: Map[Attribute, Int] = defaultSavingThrowScores(this)

  val creatureType: CreatureType = Humanoid

  val abilities: List[CombatantAbility] = standardWerewolfAbilities

  val reactionUsed: Boolean = true

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature = this
}

object Werewolf {

  val HitDice = D8

  def calculateHealth[_: RS] = (9 * HitDice) + 18

  def apply[_: RS](): Werewolf = {
    val hp = calculateHealth
    Werewolf(hp, hp)
  }

  val standardWerewolfAbilities: List[CombatantAbility] = List(
    multiAttack(1, numberOfAttacks = 2)
  )

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
    val finesse    = false

    override val hitBonus: Int = 4

    def damage(implicit rollStrategy: RollStrategy): Int = 2 * D4 // +2 from Strength
  }

  val strengthLens: Lens[Werewolf, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Werewolf, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Werewolf, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Werewolf, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Werewolf, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Werewolf, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
