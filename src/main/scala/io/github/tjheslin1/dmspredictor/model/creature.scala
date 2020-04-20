package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.classes.ranger.{Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.Armour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.monsters._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import monocle.{Lens, Optional}

sealed trait CreatureType extends Product with Serializable

case object PlayerCharacter extends CreatureType
case object Humanoid        extends CreatureType
case object Undead          extends CreatureType

trait Creature {

  val creatureType: CreatureType

  val health: Int
  val maxHealth: Int
  val stats: BaseStats
  val baseWeapon: Weapon
  def weapon[_: RS]: Weapon
  val armour: Armour
  val offHand: Option[Equipment]
  val armourClass: Int
  val damageVulnerabilities: List[DamageType]
  val damageResistances: List[DamageType]
  val damageImmunities: List[DamageType]
  val conditionResistances: List[ConditionType]
  val conditionImmunities: List[ConditionType]
  val attackStatus: AttackStatus
  val defenseStatus: AttackStatus
  val skills: Skills
  val name: String

  val abilities: List[CombatantAbility]
  val conditions: List[Condition]

  val reactionUsed: Boolean

  val reactionOnHit: Option[OnHitReaction]
  val reactionOnDamage: Option[OnDamageReaction]

  val isConscious = health > 0
  val isAlive: Boolean

  def scoresCritical(roll: Int): Boolean

  def resetStartOfTurn(): Creature

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature

  def passivePerception: Int = 10 + skills.perception
}

object Creature extends LazyLogging {

  implicit def creatureShow[_: RS]: Show[Creature] = Show.show { creature =>
    s"${creature.creatureType} - " +
      s"Name: ${creature.name}, " +
      s"health: ${creature.health}, " +
      s"AC: ${creature.armourClass}"
  }

  val creatureHealthLens: Lens[Creature, Int] = Lens[Creature, Int](_.health) { hp =>
    {
      case c: Champion => Champion._health.set(hp)(c)
      case c: Fighter  => Fighter._health.set(hp)(c)

      case c: Barbarian => Barbarian._health.set(hp)(c)
      case c: Berserker => Berserker._health.set(hp)(c)

      case c: Cleric => Cleric._health.set(hp)(c)

      case c: Rogue => Rogue._health.set(hp)(c)

      case c: Wizard => Wizard._health.set(hp)(c)

      case c: Ranger => Ranger._health.set(hp)(c)
      case c: Hunter => Hunter._health.set(hp)(c)

      case c: Goblin   => Goblin._health.set(hp)(c)
      case c: Werewolf => Werewolf._health.set(hp)(c)
      case c: Zombie   => Zombie._health.set(hp)(c)
      case c: Vampire  => Vampire._health.set(hp)(c)
      case c: Lich     => Lich._health.set(hp)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureHealthLens")
    }
  }

  val creatureMaxHealthLens: Lens[Creature, Int] = Lens[Creature, Int](_.maxHealth) { hp =>
    {
      case c: Champion => Champion._maxHealth.set(hp)(c)
      case c: Fighter  => Fighter._maxHealth.set(hp)(c)

      case c: Barbarian => Barbarian._maxHealth.set(hp)(c)
      case c: Berserker => Berserker._maxHealth.set(hp)(c)

      case c: Cleric => Cleric._maxHealth.set(hp)(c)

      case c: Rogue => Rogue._maxHealth.set(hp)(c)

      case c: Wizard => Wizard._maxHealth.set(hp)(c)

      case c: Ranger => Ranger._maxHealth.set(hp)(c)
      case c: Hunter => Hunter._maxHealth.set(hp)(c)

      case c: Goblin   => Goblin._maxHealth.set(hp)(c)
      case c: Werewolf => Werewolf._maxHealth.set(hp)(c)
      case c: Zombie   => Zombie._maxHealth.set(hp)(c)
      case c: Vampire  => Vampire._maxHealth.set(hp)(c)
      case c: Lich     => Lich._maxHealth.set(hp)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureMaxHealthLens")
    }
  }

  val creatureStatsLens: Lens[Creature, BaseStats] = Lens[Creature, BaseStats](_.stats) { stats =>
    {
      case c: Champion => Champion._stats.set(stats)(c)
      case c: Fighter  => Fighter._stats.set(stats)(c)

      case c: Barbarian => Barbarian._stats.set(stats)(c)
      case c: Berserker => Berserker._stats.set(stats)(c)

      case c: Cleric => Cleric._stats.set(stats)(c)

      case c: Rogue => Rogue._stats.set(stats)(c)

      case c: Wizard => Wizard._stats.set(stats)(c)

      case c: Ranger => Ranger._stats.set(stats)(c)
      case c: Hunter => Hunter._stats.set(stats)(c)

      case c: Goblin   => Goblin._stats.set(stats)(c)
      case c: Werewolf => Werewolf._stats.set(stats)(c)
      case c: Zombie   => Zombie._stats.set(stats)(c)
      case c: Vampire  => Vampire._stats.set(stats)(c)
      case c: Lich     => Lich._stats.set(stats)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureStatsLens")
    }
  }

  val creatureStrengthLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.strength) {
    strScore =>
      {
        case c: Champion => Champion.strengthLens.set(strScore)(c)
        case c: Fighter  => Fighter.strengthLens.set(strScore)(c)

        case c: Barbarian => Barbarian.strengthLens.set(strScore)(c)
        case c: Berserker => Berserker.strengthLens.set(strScore)(c)

        case c: Cleric => Cleric.strengthLens.set(strScore)(c)

        case c: Rogue => Rogue.strengthLens.set(strScore)(c)

        case c: Wizard => Wizard.strengthLens.set(strScore)(c)

        case c: Ranger => Ranger.strengthLens.set(strScore)(c)
        case c: Hunter => Hunter.strengthLens.set(strScore)(c)

        case c: Goblin   => Goblin.strengthLens.set(strScore)(c)
        case c: Werewolf => Werewolf.strengthLens.set(strScore)(c)
        case c: Zombie   => Zombie.strengthLens.set(strScore)(c)
        case c: Vampire  => Vampire.strengthLens.set(strScore)(c)
        case c: Lich     => Lich.strengthLens.set(strScore)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureStrengthLens")
      }
  }

  val creatureDexterityLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.dexterity) {
    dexScore =>
      {
        case c: Champion => Champion.dexterityLens.set(dexScore)(c)
        case c: Fighter  => Fighter.dexterityLens.set(dexScore)(c)

        case c: Barbarian => Barbarian.dexterityLens.set(dexScore)(c)
        case c: Berserker => Berserker.dexterityLens.set(dexScore)(c)

        case c: Cleric => Cleric.dexterityLens.set(dexScore)(c)

        case c: Rogue => Rogue.dexterityLens.set(dexScore)(c)

        case c: Wizard => Wizard.dexterityLens.set(dexScore)(c)

        case c: Ranger => Ranger.dexterityLens.set(dexScore)(c)
        case c: Hunter => Hunter.dexterityLens.set(dexScore)(c)

        case c: Goblin   => Goblin.dexterityLens.set(dexScore)(c)
        case c: Werewolf => Werewolf.dexterityLens.set(dexScore)(c)
        case c: Zombie   => Zombie.dexterityLens.set(dexScore)(c)
        case c: Vampire  => Vampire.dexterityLens.set(dexScore)(c)
        case c: Lich     => Lich.dexterityLens.set(dexScore)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureDexterityLens")
      }
  }

  val creatureConstitutionLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.constitution) {
    conScore =>
      {
        case c: Champion => Champion.constitutionLens.set(conScore)(c)
        case c: Fighter  => Fighter.constitutionLens.set(conScore)(c)

        case c: Barbarian => Barbarian.constitutionLens.set(conScore)(c)
        case c: Berserker => Berserker.constitutionLens.set(conScore)(c)

        case c: Cleric => Cleric.constitutionLens.set(conScore)(c)

        case c: Rogue => Rogue.constitutionLens.set(conScore)(c)

        case c: Wizard => Wizard.constitutionLens.set(conScore)(c)

        case c: Ranger => Ranger.constitutionLens.set(conScore)(c)
        case c: Hunter => Hunter.constitutionLens.set(conScore)(c)

        case c: Goblin   => Goblin.constitutionLens.set(conScore)(c)
        case c: Werewolf => Werewolf.constitutionLens.set(conScore)(c)
        case c: Zombie   => Zombie.constitutionLens.set(conScore)(c)
        case c: Vampire  => Vampire.constitutionLens.set(conScore)(c)
        case c: Lich     => Lich.constitutionLens.set(conScore)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureConstitutionLens")
      }
  }

  val creatureWisdomLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.wisdom) { wisScore =>
    {
      case c: Champion => Champion.wisdomLens.set(wisScore)(c)
      case c: Fighter  => Fighter.wisdomLens.set(wisScore)(c)

      case c: Barbarian => Barbarian.wisdomLens.set(wisScore)(c)
      case c: Berserker => Berserker.wisdomLens.set(wisScore)(c)

      case c: Cleric => Cleric.wisdomLens.set(wisScore)(c)

      case c: Rogue => Rogue.wisdomLens.set(wisScore)(c)

      case c: Wizard => Wizard.wisdomLens.set(wisScore)(c)

      case c: Ranger => Ranger.wisdomLens.set(wisScore)(c)
      case c: Hunter => Hunter.wisdomLens.set(wisScore)(c)

      case c: Goblin   => Goblin.wisdomLens.set(wisScore)(c)
      case c: Werewolf => Werewolf.wisdomLens.set(wisScore)(c)
      case c: Zombie   => Zombie.wisdomLens.set(wisScore)(c)
      case c: Vampire  => Vampire.wisdomLens.set(wisScore)(c)
      case c: Lich     => Lich.wisdomLens.set(wisScore)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureWisdomLens")
    }
  }

  val creatureIntelligenceLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.intelligence) {
    intScore =>
      {
        case c: Champion => Champion.intelligenceLens.set(intScore)(c)
        case c: Fighter  => Fighter.intelligenceLens.set(intScore)(c)

        case c: Barbarian => Barbarian.intelligenceLens.set(intScore)(c)
        case c: Berserker => Berserker.intelligenceLens.set(intScore)(c)

        case c: Cleric => Cleric.intelligenceLens.set(intScore)(c)

        case c: Rogue => Rogue.intelligenceLens.set(intScore)(c)

        case c: Wizard => Wizard.intelligenceLens.set(intScore)(c)

        case c: Ranger => Ranger.intelligenceLens.set(intScore)(c)
        case c: Hunter => Hunter.intelligenceLens.set(intScore)(c)

        case c: Goblin   => Goblin.intelligenceLens.set(intScore)(c)
        case c: Werewolf => Werewolf.intelligenceLens.set(intScore)(c)
        case c: Zombie   => Zombie.intelligenceLens.set(intScore)(c)
        case c: Vampire  => Vampire.intelligenceLens.set(intScore)(c)
        case c: Lich     => Lich.intelligenceLens.set(intScore)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureIntelligenceLens")
      }
  }

  val creatureCharismaLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.charisma) {
    chaScore =>
      {
        case c: Champion => Champion.charismaLens.set(chaScore)(c)
        case c: Fighter  => Fighter.charismaLens.set(chaScore)(c)

        case c: Barbarian => Barbarian.charismaLens.set(chaScore)(c)
        case c: Berserker => Berserker.charismaLens.set(chaScore)(c)

        case c: Cleric => Cleric.charismaLens.set(chaScore)(c)

        case c: Rogue => Rogue.charismaLens.set(chaScore)(c)

        case c: Wizard => Wizard.charismaLens.set(chaScore)(c)

        case c: Ranger => Ranger.charismaLens.set(chaScore)(c)
        case c: Hunter => Hunter.charismaLens.set(chaScore)(c)

        case c: Goblin   => Goblin.charismaLens.set(chaScore)(c)
        case c: Werewolf => Werewolf.charismaLens.set(chaScore)(c)
        case c: Zombie   => Zombie.charismaLens.set(chaScore)(c)
        case c: Vampire  => Vampire.charismaLens.set(chaScore)(c)
        case c: Lich     => Lich.charismaLens.set(chaScore)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureCharismaLens")
      }
  }

  val creatureBaseWeaponLens: Lens[Creature, Weapon] = Lens[Creature, Weapon](_.baseWeapon) { wpn =>
    {
      case c: Champion => Champion._baseWeapon.set(wpn)(c)
      case c: Fighter  => Fighter._baseWeapon.set(wpn)(c)

      case c: Barbarian => Barbarian._baseWeapon.set(wpn)(c)
      case c: Berserker => Berserker._baseWeapon.set(wpn)(c)

      case c: Cleric => Cleric._baseWeapon.set(wpn)(c)

      case c: Rogue => Rogue._baseWeapon.set(wpn)(c)

      case c: Wizard => Wizard._baseWeapon.set(wpn)(c)

      case c: Ranger => Ranger._baseWeapon.set(wpn)(c)
      case c: Hunter => Hunter._baseWeapon.set(wpn)(c)

      case c: Goblin   => Goblin._baseWeapon.set(wpn)(c)
      case c: Werewolf => Werewolf._baseWeapon.set(wpn)(c)
      case c: Zombie   => Zombie._baseWeapon.set(wpn)(c)
      case c: Vampire  => Vampire._baseWeapon.set(wpn)(c)
      case c: Lich     => Lich._baseWeapon.set(wpn)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureBaseWeaponLens")
    }
  }

  val creatureArmourLens: Lens[Creature, Armour] = Lens[Creature, Armour](_.armour) { armr =>
    {
      case c: Champion => Champion._armour.set(armr)(c)
      case c: Fighter  => Fighter._armour.set(armr)(c)

      case c: Barbarian => Barbarian._armour.set(armr)(c)
      case c: Berserker => Berserker._armour.set(armr)(c)

      case c: Cleric => Cleric._armour.set(armr)(c)

      case c: Rogue => Rogue._armour.set(armr)(c)

      case c: Wizard => Wizard._armour.set(armr)(c)

      case c: Ranger => Ranger._armour.set(armr)(c)
      case c: Hunter => Hunter._armour.set(armr)(c)

      case c: Goblin   => Goblin._armour.set(armr)(c)
      case c: Werewolf => Werewolf._armour.set(armr)(c)
      case c: Zombie   => Zombie._armour.set(armr)(c)
      case c: Vampire  => Vampire._armour.set(armr)(c)
      case c: Lich     => Lich._armour.set(armr)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureArmourLens")
    }
  }

  val creatureOffHandLens: Lens[Creature, Option[Equipment]] =
    Lens[Creature, Option[Equipment]](_.offHand) { offH =>
      {
        case c: Champion => Champion._offHand.set(offH)(c)
        case c: Fighter  => Fighter._offHand.set(offH)(c)

        case c: Barbarian => Barbarian._offHand.set(offH)(c)
        case c: Berserker => Berserker._offHand.set(offH)(c)

        case c: Cleric => Cleric._offHand.set(offH)(c)

        case c: Rogue => Rogue._offHand.set(offH)(c)

        case c: Wizard => Wizard._offHand.set(offH)(c)

        case c: Ranger => Ranger._offHand.set(offH)(c)
        case c: Hunter => Hunter._offHand.set(offH)(c)

        case c: Goblin   => Goblin._offHand.set(offH)(c)
        case c: Werewolf => Werewolf._offHand.set(offH)(c)
        case c: Zombie   => Zombie._offHand.set(offH)(c)
        case c: Vampire  => Vampire._offHand.set(offH)(c)
        case c: Lich     => Lich._offHand.set(offH)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureOffHandLens")
      }
    }

  val creatureDamageVulnerabilitiesLens: Lens[Creature, List[DamageType]] =
    Lens[Creature, List[DamageType]](_.damageVulnerabilities) { res =>
      {
        case c: Champion => Champion._damageVulnerabilities.set(res)(c)
        case c: Fighter  => Fighter._damageVulnerabilities.set(res)(c)

        case c: Barbarian => Barbarian._damageVulnerabilities.set(res)(c)
        case c: Berserker => Berserker._damageVulnerabilities.set(res)(c)

        case c: Cleric => Cleric._damageVulnerabilities.set(res)(c)

        case c: Rogue => Rogue._damageVulnerabilities.set(res)(c)

        case c: Wizard => Wizard._damageVulnerabilities.set(res)(c)

        case c: Ranger => Ranger._damageVulnerabilities.set(res)(c)
        case c: Hunter => Hunter._damageVulnerabilities.set(res)(c)

        case c: Goblin   => Goblin._damageVulnerabilities.set(res)(c)
        case c: Werewolf => Werewolf._damageVulnerabilities.set(res)(c)
        case c: Zombie   => Zombie._damageVulnerabilities.set(res)(c)
        case c: Vampire  => Vampire._damageVulnerabilities.set(res)(c)
        case c: Lich     => Lich._damageVulnerabilities.set(res)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureDamageResistancesLens")
      }
    }

  val creatureDamageResistancesLens: Lens[Creature, List[DamageType]] =
    Lens[Creature, List[DamageType]](_.damageResistances) { res =>
      {
        case c: Champion => Champion._damageResistances.set(res)(c)
        case c: Fighter  => Fighter._damageResistances.set(res)(c)

        case c: Barbarian => Barbarian._damageResistances.set(res)(c)
        case c: Berserker => Berserker._damageResistances.set(res)(c)

        case c: Cleric => Cleric._damageResistances.set(res)(c)

        case c: Rogue => Rogue._damageResistances.set(res)(c)

        case c: Wizard => Wizard._damageResistances.set(res)(c)

        case c: Ranger => Ranger._damageResistances.set(res)(c)
        case c: Hunter => Hunter._damageResistances.set(res)(c)

        case c: Goblin   => Goblin._damageResistances.set(res)(c)
        case c: Werewolf => Werewolf._damageResistances.set(res)(c)
        case c: Zombie   => Zombie._damageResistances.set(res)(c)
        case c: Vampire  => Vampire._damageResistances.set(res)(c)
        case c: Lich     => Lich._damageResistances.set(res)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureDamageResistancesLens")
      }
    }

  val creatureDamageImmunitiesLens: Lens[Creature, List[DamageType]] =
    Lens[Creature, List[DamageType]](_.damageImmunities) { res =>
      {
        case c: Champion => Champion._damageImmunities.set(res)(c)
        case c: Fighter  => Fighter._damageImmunities.set(res)(c)

        case c: Barbarian => Barbarian._damageImmunities.set(res)(c)
        case c: Berserker => Berserker._damageImmunities.set(res)(c)

        case c: Cleric => Cleric._damageImmunities.set(res)(c)

        case c: Rogue => Rogue._damageImmunities.set(res)(c)

        case c: Wizard => Wizard._damageImmunities.set(res)(c)

        case c: Ranger => Ranger._damageImmunities.set(res)(c)
        case c: Hunter => Hunter._damageImmunities.set(res)(c)

        case c: Goblin   => Goblin._damageImmunities.set(res)(c)
        case c: Werewolf => Werewolf._damageImmunities.set(res)(c)
        case c: Zombie   => Zombie._damageImmunities.set(res)(c)
        case c: Vampire  => Vampire._damageImmunities.set(res)(c)
        case c: Lich     => Lich._damageImmunities.set(res)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureDamageImmunitiesLens")
      }
    }

  val creatureConditionResistancesLens: Lens[Creature, List[ConditionType]] =
    Lens[Creature, List[ConditionType]](_.conditionResistances) { res =>
      {
        case c: Champion => Champion._conditionResistances.set(res)(c)
        case c: Fighter  => Fighter._conditionResistances.set(res)(c)

        case c: Barbarian => Barbarian._conditionResistances.set(res)(c)
        case c: Berserker => Berserker._conditionResistances.set(res)(c)

        case c: Cleric => Cleric._conditionResistances.set(res)(c)

        case c: Rogue => Rogue._conditionResistances.set(res)(c)

        case c: Wizard => Wizard._conditionResistances.set(res)(c)

        case c: Ranger => Ranger._conditionResistances.set(res)(c)
        case c: Hunter => Hunter._conditionResistances.set(res)(c)

        case c: Goblin   => Goblin._conditionResistances.set(res)(c)
        case c: Werewolf => Werewolf._conditionResistances.set(res)(c)
        case c: Zombie   => Zombie._conditionResistances.set(res)(c)
        case c: Vampire  => Vampire._conditionResistances.set(res)(c)
        case c: Lich     => Lich._conditionResistances.set(res)(c)

        case _ =>
          throw new NotImplementedError("Missing a case in creatureConditionResistancesLens")
      }
    }

  val creatureConditionImmunitiesLens: Lens[Creature, List[ConditionType]] =
    Lens[Creature, List[ConditionType]](_.conditionImmunities) { res =>
      {
        case c: Champion => Champion._conditionImmunities.set(res)(c)
        case c: Fighter  => Fighter._conditionImmunities.set(res)(c)

        case c: Barbarian => Barbarian._conditionImmunities.set(res)(c)
        case c: Berserker => Berserker._conditionImmunities.set(res)(c)

        case c: Cleric => Cleric._conditionImmunities.set(res)(c)

        case c: Rogue => Rogue._conditionImmunities.set(res)(c)

        case c: Wizard => Wizard._conditionImmunities.set(res)(c)

        case c: Ranger => Ranger._conditionImmunities.set(res)(c)
        case c: Hunter => Hunter._conditionImmunities.set(res)(c)

        case c: Goblin   => Goblin._conditionImmunities.set(res)(c)
        case c: Werewolf => Werewolf._conditionImmunities.set(res)(c)
        case c: Zombie   => Zombie._conditionImmunities.set(res)(c)
        case c: Vampire  => Vampire._conditionImmunities.set(res)(c)
        case c: Lich     => Lich._conditionImmunities.set(res)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureConditionImmunitiesLens")
      }
    }

  val creatureAbilitiesLens: Lens[Creature, List[CombatantAbility]] =
    Lens[Creature, List[CombatantAbility]](_.abilities) { res =>
      {
        case c: Champion => Champion._abilities.set(res)(c)
        case c: Fighter  => Fighter._abilities.set(res)(c)

        case c: Barbarian => Barbarian._abilities.set(res)(c)
        case c: Berserker => Berserker._abilities.set(res)(c)

        case c: Cleric => Cleric._abilities.set(res)(c)

        case c: Rogue => Rogue._abilities.set(res)(c)

        case c: Wizard => Wizard._abilities.set(res)(c)

        case c: Ranger => Ranger._abilities.set(res)(c)
        case c: Hunter => Hunter._abilities.set(res)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureAbilitiesLens")
      }
    }

  val creatureConditionsLens: Lens[Creature, List[Condition]] =
    Lens[Creature, List[Condition]](_.conditions) { conditions =>
      {
        case c: Champion => Champion._conditions.set(conditions)(c)
        case c: Fighter  => Fighter._conditions.set(conditions)(c)

        case c: Barbarian => Barbarian._conditions.set(conditions)(c)
        case c: Berserker => Berserker._conditions.set(conditions)(c)

        case c: Cleric => Cleric._conditions.set(conditions)(c)

        case c: Rogue => Rogue._conditions.set(conditions)(c)

        case c: Wizard => Wizard._conditions.set(conditions)(c)

        case c: Ranger => Ranger._conditions.set(conditions)(c)
        case c: Hunter => Hunter._conditions.set(conditions)(c)

        case c: Goblin   => Goblin._conditions.set(conditions)(c)
        case c: Werewolf => Werewolf._conditions.set(conditions)(c)
        case c: Zombie   => Zombie._conditions.set(conditions)(c)
        case c: Vampire  => Vampire._conditions.set(conditions)(c)
        case c: Lich     => Lich._conditions.set(conditions)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureConditionsLens")
      }
    }

  val creatureReactionUsedLens: Lens[Creature, Boolean] =
    Lens[Creature, Boolean](_.reactionUsed) { reactionUsed =>
      {
        case c: Champion => Champion._reactionUsed.set(reactionUsed)(c)
        case c: Fighter  => Fighter._reactionUsed.set(reactionUsed)(c)

        case c: Barbarian => Barbarian._reactionUsed.set(reactionUsed)(c)
        case c: Berserker => Berserker._reactionUsed.set(reactionUsed)(c)

        case c: Cleric => Cleric._reactionUsed.set(reactionUsed)(c)

        case c: Rogue => Rogue._reactionUsed.set(reactionUsed)(c)

        case c: Wizard => Wizard._reactionUsed.set(reactionUsed)(c)

        case c: Ranger => Ranger._reactionUsed.set(reactionUsed)(c)
        case c: Hunter => Hunter._reactionUsed.set(reactionUsed)(c)

        case c: Goblin   => Goblin._reactionUsed.set(reactionUsed)(c)
        case c: Werewolf => Werewolf._reactionUsed.set(reactionUsed)(c)
        case c: Zombie   => Zombie._reactionUsed.set(reactionUsed)(c)
        case c: Vampire  => Vampire._reactionUsed.set(reactionUsed)(c)
        case c: Lich     => Lich._reactionUsed.set(reactionUsed)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureReactionUsedLens")
      }
    }

  val creatureAttackStatusLens: Lens[Creature, AttackStatus] =
    Lens[Creature, AttackStatus](_.attackStatus) { status =>
      {
        case c: Champion => Champion._attackStatus.set(status)(c)
        case c: Fighter  => Fighter._attackStatus.set(status)(c)

        case c: Barbarian => Barbarian._attackStatus.set(status)(c)
        case c: Berserker => Berserker._attackStatus.set(status)(c)

        case c: Cleric => Cleric._attackStatus.set(status)(c)

        case c: Rogue => Rogue._attackStatus.set(status)(c)

        case c: Wizard => Wizard._attackStatus.set(status)(c)

        case c: Ranger => Ranger._attackStatus.set(status)(c)
        case c: Hunter => Hunter._attackStatus.set(status)(c)

        case c: Goblin   => Goblin._attackStatus.set(status)(c)
        case c: Werewolf => Werewolf._attackStatus.set(status)(c)
        case c: Zombie   => Zombie._attackStatus.set(status)(c)
        case c: Vampire  => Vampire._attackStatus.set(status)(c)
        case c: Lich     => Lich._attackStatus.set(status)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureAttackStatusLens")
      }
    }

  val creatureDefenseStatusLens: Lens[Creature, AttackStatus] =
    Lens[Creature, AttackStatus](_.defenseStatus) { status =>
      {
        case c: Champion => Champion._defenseStatus.set(status)(c)
        case c: Fighter  => Fighter._defenseStatus.set(status)(c)

        case c: Barbarian => Barbarian._defenseStatus.set(status)(c)
        case c: Berserker => Berserker._defenseStatus.set(status)(c)

        case c: Cleric => Cleric._defenseStatus.set(status)(c)

        case c: Rogue => Rogue._defenseStatus.set(status)(c)

        case c: Wizard => Wizard._defenseStatus.set(status)(c)

        case c: Ranger => Ranger._defenseStatus.set(status)(c)
        case c: Hunter => Hunter._defenseStatus.set(status)(c)

        case c: Goblin   => Goblin._defenseStatus.set(status)(c)
        case c: Werewolf => Werewolf._defenseStatus.set(status)(c)
        case c: Zombie   => Zombie._defenseStatus.set(status)(c)
        case c: Vampire  => Vampire._defenseStatus.set(status)(c)
        case c: Lich     => Lich._defenseStatus.set(status)(c)

        case _ => throw new NotImplementedError("Missing a case in creatureDefenseStatusLens")
      }
    }

  val creatureIsAliveLens: Lens[Creature, Boolean] = Lens[Creature, Boolean](_.isAlive) { isAlive =>
    {
      case c: Champion => Champion._isAlive.set(isAlive)(c)
      case c: Fighter  => Fighter._isAlive.set(isAlive)(c)

      case c: Barbarian => Barbarian._isAlive.set(isAlive)(c)
      case c: Berserker => Berserker._isAlive.set(isAlive)(c)

      case c: Cleric => Cleric._isAlive.set(isAlive)(c)

      case c: Rogue => Rogue._isAlive.set(isAlive)(c)

      case c: Wizard => Wizard._isAlive.set(isAlive)(c)

      case c: Ranger => Ranger._isAlive.set(isAlive)(c)
      case c: Hunter => Hunter._isAlive.set(isAlive)(c)

      case c: Goblin   => Goblin._isAlive.set(isAlive)(c)
      case c: Werewolf => Werewolf._isAlive.set(isAlive)(c)
      case c: Zombie   => Zombie._isAlive.set(isAlive)(c)
      case c: Vampire  => Vampire._isAlive.set(isAlive)(c)
      case c: Lich     => Lich._isAlive.set(isAlive)(c)

      case _ => throw new NotImplementedError("Missing a case in creatureIsAliveLens")
    }
  }

  val creatureSkillsOptional: Optional[Creature, Skills] = Optional[Creature, Skills] {
    case c: Champion => c.skills.some
    case c: Fighter  => c.skills.some

    case c: Barbarian => c.skills.some
    case c: Berserker => c.skills.some

    case c: Cleric => c.skills.some
    case c: Rogue  => c.skills.some

    case c: Wizard => c.skills.some

    case c: Ranger => c.skills.some

    case _ => none[Skills]
  } { skills =>
    {
      case c: Champion => Champion._skills.set(skills)(c)
      case c: Fighter  => Fighter._skills.set(skills)(c)

      case c: Barbarian => Barbarian._skills.set(skills)(c)
      case c: Berserker => Berserker._skills.set(skills)(c)

      case c: Cleric => Cleric._skills.set(skills)(c)

      case c: Rogue => Rogue._skills.set(skills)(c)

      case c: Wizard => Wizard._skills.set(skills)(c)

      case c: Ranger => Ranger._skills.set(skills)(c)
      case c: Hunter => Hunter._skills.set(skills)(c)

      case c: Creature => c
    }
  }
}
