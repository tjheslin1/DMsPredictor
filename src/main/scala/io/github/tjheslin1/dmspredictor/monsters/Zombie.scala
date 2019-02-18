package io.github.tjheslin1.dmspredictor.monsters

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Zombie._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Zombie(health: Int,
                               maxHealth: Int,
                               stats: BaseStats = BaseStats(13, 6, 16, 3, 6, 5),
                               armourClass: Int = 8,
                               baseWeapon: Weapon = Slam,
                               armour: Armour = NoArmour,
                               offHand: Option[Equipment] = None,
                               conditions: List[Condition] = List.empty,
                               resistances: List[DamageType] = List.empty,
                               immunities: List[DamageType] = List(Poison),
                               attackStatus: AttackStatus = Regular,
                               defenseStatus: AttackStatus = Regular,
                               name: String = NameGenerator.randomName)
    extends Creature
    with LazyLogging {

  val creatureType: CreatureType         = Monster
  val proficiencyBonus: ProficiencyBonus = 0

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int,
                          damageType: DamageType,
                          attackResult: AttackResult): Creature = {
    val adjustedDmg = Creature.adjustedDamage(dmg, damageType, this)
    if ((health - adjustedDmg) <= 0 && attackResult != CriticalHit && damageType != Radiant) {

      val dc = 5 + adjustedDmg
      if (savingThrowPassed(dc, Constitution, this)) {
        logger.debug("Zombie used Undead Fortitude to remain at 1 hp")
        _health.set(1)(this)
      } else _health.set(Math.max(0, health - adjustedDmg))(this)
    } else _health.set(Math.max(0, health - adjustedDmg))(this)
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val abilities: List[CombatantAbility] = List.empty

  def resetStartOfTurn(): Creature = this
}

object Zombie {

  def apply[_: RS](): Zombie = {
    val hp = (3 * D8) + 9
    Zombie(hp, hp)
  }

  case object Slam extends Weapon {

    val name: String           = "Slam"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Bludgeoning
    val twoHanded: Boolean     = true

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
