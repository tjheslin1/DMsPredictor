package io.github.tjheslin1.dmspredictor.monsters.vampire

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.monsters.{Legendary, Monster}
import io.github.tjheslin1.dmspredictor.monsters.MonsterAbilities.multiAttack
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire._
import io.github.tjheslin1.dmspredictor.monsters.vampire.VampireAbilities._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Vampire(
    health: Int,
    maxHealth: Int,
    stats: BaseStats = BaseStats(18, 18, 18, 17, 15, 18),
    armourClass: Int = 12,
    baseWeapon: Weapon = UnarmedStrike,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List(Necrotic, Bludgeoning, Piercing, Slashing),
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    conditions: List[Condition] = List.empty,
    reactionUsed: Boolean = false,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    radiantDamageTaken: Boolean = false,
    firstAttack: Boolean = true,
    biteUsed: Boolean = false,
    isAlive: Boolean = true,
    legendaryResistances: Int = 3,
    name: String = NameGenerator.randomName
) extends Monster
    with Legendary
    with LazyLogging {

  val challengeRating = 13.0
  val skills          = Skills(perception = 7, stealth = 9)

  val savingThrowScores: Map[Attribute, Int] = Map(
    Strength     -> mod(stats.strength),
    Dexterity    -> 9,
    Constitution -> mod(stats.constitution),
    Wisdom       -> 7,
    Intelligence -> mod(stats.intelligence),
    Charisma     -> 9
  )

  val creatureType: CreatureType = Undead

  val abilities: List[CombatantAbility] = vampireAbilities

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    damageType match {
      case Radiant =>
        val updatedVampire = applyDamage(this, dmg).asInstanceOf[Vampire]

        if (dmg > 0) _radiantDamageTaken.set(true)(updatedVampire)
        else _radiantDamageTaken.set(false)(updatedVampire)
      case _ => applyDamage(this, adjustedDamage(dmg, damageType, this))
    }

  def restoreHealth(healing: Int): Creature = copy(health = Math.min(maxHealth, health + healing))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature = {
    val radiantCalculatedVampire =
      if (radiantDamageTaken) copy(radiantDamageTaken = false)
      else {
        val regeneratedHp = Math.min(maxHealth, health + 20)

        logger.debug(s"$name regenerated health")

        Creature.creatureHealthLens
          .set(regeneratedHp)(copy(radiantDamageTaken = false))
          .asInstanceOf[Vampire]
      }

    radiantCalculatedVampire.copy(biteUsed = false, firstAttack = true)
  }
}

object Vampire {

  val CharmDC = 17

  def calculateHealth[_: RS](): Int = (17 * D8) + 68

  val vampireAbilities: List[CombatantAbility] = List(
    charm(1),
    multiAttack(2, numberOfAttacks = 2),
    bite(3),
    unarmedStrike(4)
  )

  case object UnarmedStrike extends Weapon {
    val name: String           = "Unarmed Strike (Vampire)"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Bludgeoning
    val twoHanded: Boolean     = true
    val finesse                = false

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
