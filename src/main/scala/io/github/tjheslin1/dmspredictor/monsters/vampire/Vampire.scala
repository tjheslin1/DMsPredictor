package io.github.tjheslin1.dmspredictor.monsters.vampire

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.monsters.MonsterAbilities.multiAttack
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire._
import io.github.tjheslin1.dmspredictor.monsters.vampire.VampireAbilities._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Vampire(health: Int,
                                maxHealth: Int,
                                stats: BaseStats = BaseStats(18, 18, 18, 17, 15, 18),
                                armourClass: Int = 12,
                                baseWeapon: Weapon = UnarmedStrike,
                                armour: Armour = NoArmour,
                                offHand: Option[Equipment] = None,
                                resistances: List[DamageType] =
                                  List(Necrotic, Bludgeoning, Piercing, Slashing),
                                immunities: List[DamageType] = List.empty[DamageType],
                                conditions: List[Condition] = List.empty,
                                attackStatus: AttackStatus = Regular,
                                defenseStatus: AttackStatus = Regular,
                                radiantDamageTaken: Boolean = false,
                                firstAttack: Boolean = true,
                                biteUsed: Boolean = false,
                                name: String = NameGenerator.randomName)
    extends Monster {

  val challengeRating: Double = 13.0

  val creatureType: CreatureType = Undead

  val abilities: List[CombatantAbility] = vampireAbilities

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    damageType match {
      case Radiant =>
        copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)),
             radiantDamageTaken = true)
      case _ => copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))
    }

  def restoreHealth(healing: Int): Creature = copy(health = Math.min(maxHealth, health + healing))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature =
    if (radiantDamageTaken) copy(radiantDamageTaken = false)
    else {
      val regeneratedHp = Math.min(maxHealth, health + 20)
      Creature.creatureHealthLens.set(regeneratedHp)(copy(radiantDamageTaken = false))
    }
}

object Vampire {

  val TotalAttacks = 2

  val vampireAbilities: List[CombatantAbility] = List(
    multiAttack(1, TotalAttacks),
    bite(2),
    unarmedStrike(3)
  )

  case object UnarmedStrike extends Weapon {
    val name: String           = "Unarmed Strike (Vampire)"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Bludgeoning
    val twoHanded: Boolean     = true

    override val hitBonus: Int = 9

    def damage(implicit rollStrategy: RollStrategy): Int = 1 * D8 // +4 from Strength

    val GrappleDc = 18
  }

  val strengthLens: Lens[Vampire, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Vampire, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Vampire, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Vampire, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Vampire, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Vampire, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
