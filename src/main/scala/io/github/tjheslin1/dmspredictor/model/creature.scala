package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.monsters._
import monocle.{Lens, Optional}

sealed trait CreatureType

case object EnemyMonster    extends CreatureType
case object PlayerCharacter extends CreatureType

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
  val proficiencyBonus: ProficiencyBonus
  val resistances: List[DamageType]
  val immunities: List[DamageType]
  val name: String

  val isConscious = health > 0

  def updateHealth(modification: Int): Creature

  val abilities: List[CreatureAbility]
}

object Creature {

  implicit val determineCritical: DetermineCritical[Creature] = new DetermineCritical[Creature] {
    def attackIsCritical(creature: Creature, roll: Int): Boolean = roll == 20
  }

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

      case c: Goblin   => Goblin._health.set(hp)(c)
      case c: Werewolf => Werewolf._health.set(hp)(c)
    }
  }

  val creatureMaxHealthLens: Lens[Creature, Int] = Lens[Creature, Int](_.maxHealth) { hp =>
    {
      case c: Champion => Champion._maxHealth.set(hp)(c)
      case c: Fighter  => Fighter._maxHealth.set(hp)(c)

      case c: Goblin   => Goblin._maxHealth.set(hp)(c)
      case c: Werewolf => Werewolf._maxHealth.set(hp)(c)
    }
  }

  val creatureStatsLens: Lens[Creature, BaseStats] = Lens[Creature, BaseStats](_.stats) { stats =>
    {
      case c: Champion => Champion._stats.set(stats)(c)
      case c: Fighter  => Fighter._stats.set(stats)(c)

      case c: Goblin   => Goblin._stats.set(stats)(c)
      case c: Werewolf => Werewolf._stats.set(stats)(c)
    }
  }

  val creatureStrengthLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.strength) { strScore =>
    {
      case c: Champion => Champion.strengthLens.set(strScore)(c)
      case c: Fighter  => Fighter.strengthLens.set(strScore)(c)

      case c: Goblin   => Goblin.strengthLens.set(strScore)(c)
      case c: Werewolf => Werewolf.strengthLens.set(strScore)(c)
    }
  }

  val creatureDexterityLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.dexterity) { dexScore =>
    {
      case c: Champion => Champion.dexterityLens.set(dexScore)(c)
      case c: Fighter  => Fighter.dexterityLens.set(dexScore)(c)

      case c: Goblin   => Goblin.dexterityLens.set(dexScore)(c)
      case c: Werewolf => Werewolf.dexterityLens.set(dexScore)(c)
    }
  }

  val creatureConstitutionLens: Lens[Creature, Stat] = Lens[Creature, Stat](_.stats.constitution) { conScore =>
    {
      case c: Champion => Champion.constitutionLens.set(conScore)(c)
      case c: Fighter  => Fighter.constitutionLens.set(conScore)(c)

      case c: Goblin   => Goblin.constitutionLens.set(conScore)(c)
      case c: Werewolf => Werewolf.constitutionLens.set(conScore)(c)
    }
  }

  val creatureBaseWeaponLens: Lens[Creature, Weapon] = Lens[Creature, Weapon](_.baseWeapon) { wpn =>
    {
      case c: Champion => Champion._baseWeapon.set(wpn)(c)
      case c: Fighter  => Fighter._baseWeapon.set(wpn)(c)

      case c: Goblin   => Goblin._baseWeapon.set(wpn)(c)
      case c: Werewolf => Werewolf._baseWeapon.set(wpn)(c)
    }
  }

  val creatureArmourLens: Lens[Creature, Armour] = Lens[Creature, Armour](_.armour) { armr =>
    {
      case c: Champion => Champion._armour.set(armr)(c)
      case c: Fighter  => Fighter._armour.set(armr)(c)

      case c: Goblin   => Goblin._armour.set(armr)(c)
      case c: Werewolf => Werewolf._armour.set(armr)(c)
    }
  }

  val creatureOffHandLens: Lens[Creature, Option[Equipment]] = Lens[Creature, Option[Equipment]](_.offHand) { offH =>
    {
      case c: Champion => Champion._offHand.set(offH)(c)
      case c: Fighter  => Fighter._offHand.set(offH)(c)

      case c: Goblin   => Goblin._offHand.set(offH)(c)
      case c: Werewolf => Werewolf._offHand.set(offH)(c)
    }
  }

  val creatureArmourClassOptional: Optional[Creature, Int] = Optional[Creature, Int] {
    case c: Goblin   => c.armourClass.some
    case c: Werewolf => c.armourClass.some
    case _           => none[Int]
  } { ac =>
    {
      case c: Goblin   => Goblin._armourClass.set(ac)(c)
      case c: Werewolf => Werewolf._armourClass.set(ac)(c)
      case c: Creature => c
    }
  }

  val creatureProficiencyBonusOptional: Optional[Creature, ProficiencyBonus] = Optional[Creature, ProficiencyBonus] {
    case c: Champion => val pb: ProficiencyBonus = c.proficiencyBonus; pb.some
    case c: Fighter  => val pb: ProficiencyBonus = c.proficiencyBonus; pb.some
    case _           => println("***** NO PROF B FOUND!"); none[ProficiencyBonus]
  } { profBonus =>
    {
      case c: Champion => Champion._proficiencyBonus.set(profBonus)(c)
      case c: Fighter  => Fighter._proficiencyBonus.set(profBonus)(c)
      case c: Creature => println("***** NO UPDATE MADE TO PROF B!"); c
    }
  }

  val creatureResistancesLens: Lens[Creature, List[DamageType]] = Lens[Creature, List[DamageType]](_.resistances) {
    res =>
      {
        case c: Champion => Champion._resistances.set(res)(c)
        case c: Fighter  => Fighter._resistances.set(res)(c)

        case c: Goblin   => Goblin._resistances.set(res)(c)
        case c: Werewolf => Werewolf._resistances.set(res)(c)
      }
  }

  val creatureImmunitiesLens: Lens[Creature, List[DamageType]] = Lens[Creature, List[DamageType]](_.immunities) { res =>
    {
      case c: Champion => Champion._immunities.set(res)(c)
      case c: Fighter  => Fighter._immunities.set(res)(c)

      case c: Goblin   => Goblin._immunities.set(res)(c)
      case c: Werewolf => Werewolf._immunities.set(res)(c)
    }
  }

  val creatureAbilitiesLens: Lens[Creature, List[CreatureAbility]] =
    Lens[Creature, List[CreatureAbility]](_.abilities) { res =>
      {
        case c: Champion => Champion._abilities.set(res)(c)
        case c: Fighter  => Fighter._abilities.set(res)(c)

        case c: Goblin   => Goblin._abilities.set(res)(c)
        case c: Werewolf => Werewolf._abilities.set(res)(c)
      }
    }

  val creatureLevelOptional: Optional[Creature, Level] = Optional[Creature, Level] {
    case c: Champion => val lvl: Level = c.level; lvl.some
    case c: Fighter  => val lvl: Level = c.level; lvl.some
    case _           => println("***** NO LEVEL UPDATED!"); none[Level]
  } { lvl =>
    {
      case c: Champion => Champion._level.set(lvl)(c)
      case c: Fighter  => Fighter._level.set(lvl)(c)
      case c: Creature => println("***** NO UPDATE MADE TO LEVEL!"); c
    }
  }
}
