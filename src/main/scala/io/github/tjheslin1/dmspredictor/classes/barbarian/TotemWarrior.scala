package io.github.tjheslin1.dmspredictor.classes.barbarian

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian._
import io.github.tjheslin1.dmspredictor.classes.barbarian.TotemWarrior.Totem
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class TotemWarrior(level: Level,
                                     health: Int,
                                     maxHealth: Int,
                                     stats: BaseStats,
                                     baseWeapon: Weapon,
                                     rageUsages: Int,
                                     totem: Totem,
                                     abilities: List[CombatantAbility],
                                     armour: Armour = NoArmour,
                                     offHand: Option[Equipment] = None,
                                     proficiencyBonus: ProficiencyBonus = 0,
                                     resistances: List[DamageType] = List.empty,
                                     immunities: List[DamageType] = List.empty,
                                     bonusActionUsed: Boolean = false,
                                     attackStatus: AttackStatus = Regular,
                                     defenseStatus: AttackStatus = Regular,
                                     inRage: Boolean = false,
                                     rageTurnsLeft: Int = 10,
                                     name: String = NameGenerator.randomName)
    extends BaseBarbarian {

  def weapon[_: RS]: Weapon = weaponWithRageDamage(baseWeapon, inRage)

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth(modification: Int): Creature = copy(health = Math.max(0, health + modification))

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object TotemWarrior {

  import BaseBarbarianAbilities._

  def rageResistances(totem: Totem): List[DamageType] = totem match {
    case Bear =>
      List(Bludgeoning, Piercing, Slashing, Magical, Acid, Cold, Fire, Lightning, Poison, Thunder)
  }

  def standardTotemWarriorAbilities(totem: Totem): List[CombatantAbility] = List(
    rage(1, rageResistances = rageResistances(totem)),
    extraAttack(2),
    recklessAttack(3)
  )

  implicit def totemWarriorShow[_: RS]: Show[TotemWarrior] = Show.show { totemWarrior =>
    s"Totem Warrior: " +
      s"Name: ${totemWarrior.name}, " +
      s"health: ${totemWarrior.health}, " +
      s"AC: ${totemWarrior.armourClass}"
  }

  val strengthLens: Lens[TotemWarrior, Stat]  = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TotemWarrior, Stat] = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TotemWarrior, Stat] = _stats composeLens GenLens[BaseStats](
    _.constitution)
  val wisdomLens: Lens[TotemWarrior, Stat] = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[TotemWarrior, Stat] = _stats composeLens GenLens[BaseStats](
    _.intelligence)
  val charismaLens: Lens[TotemWarrior, Stat] = _stats composeLens GenLens[BaseStats](_.charisma)

  trait Totem extends Product with Serializable

  case object Bear extends Totem
}
