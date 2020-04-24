package io.github.tjheslin1.dmspredictor.monsters

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.monsters.Monster.defaultSavingThrowScores
import io.github.tjheslin1.dmspredictor.monsters.Zombie._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Zombie(
    health: Int,
    maxHealth: Int,
    stats: BaseStats = BaseStats(13, 6, 16, 3, 6, 5),
    armourClass: Int = 8,
    baseWeapon: Weapon = Slam,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List(Poison),
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List(PoisonedCondition),
    conditions: List[Condition] = List.empty[Condition],
    reactionUsed: Boolean = false,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends Monster
    with LazyLogging {

  val challengeRating: Double                = 0.25
  val skills                                 = Skills(perception = 0, stealth = 0)
  val savingThrowScores: Map[Attribute, Int] = defaultSavingThrowScores(this)

  val creatureType: CreatureType = Undead

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val adjustedDmg = adjustedDamage(dmg, damageType, this)
    if ((health - adjustedDmg) <= 0 && attackResult == Hit && damageType != Radiant) {

      val dc = 5 + adjustedDmg
      if (savingThrowPassed(dc, Constitution, this)) {
        logger.debug("Zombie used Undead Fortitude to remain at 1 hp")
        _health.set(1)(this)
      } else applyDamage(this, adjustedDmg)
    } else applyDamage(this, adjustedDmg)
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val abilities: List[CombatantAbility] = List.empty

  def resetStartOfTurn(): Creature = this
}

object Zombie {

  def calculateHealth[_: RS](): Int = (3 * D8) + 9

  def apply[_: RS](): Zombie = {
    val hp = calculateHealth()
    Zombie(hp, hp)
  }

  def withName[_: RS](zombieName: String): Zombie = {
    val hp = calculateHealth()
    Zombie(hp, hp, name = zombieName)
  }

  case object Slam extends Weapon {

    val name: String           = "Slam"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Bludgeoning
    val twoHanded: Boolean     = true
    val finesse                = false

    override val hitBonus: Int = 3

    def damage(implicit rollStrategy: RollStrategy): Int = (1 * D6) + 1
  }

  val strengthLens: Lens[Zombie, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Zombie, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Zombie, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Zombie, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Zombie, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Zombie, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
