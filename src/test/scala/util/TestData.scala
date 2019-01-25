package util

import cats.syntax.option._
import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.standardCoreAbilities
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.{Fighter, _}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.ChromaticOrb
import io.github.tjheslin1.dmspredictor.model.spellcasting.{FirstLevelSpellSlot, Spell}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

object TestData {

  val DamageTypes = List(Bludgeoning, Piercing, Slashing)

  implicit class CombatantOps(val combatant: Combatant) extends AnyVal {
    import Combatant._

    def withCreature(c: Creature) = creatureLens.set(c)(combatant)
  }

  implicit class TestMonsterOps(val testMonster: TestMonster) extends AnyVal {
    import TestMonster._

    def withName(creatureName: String)           = _name.set(creatureName)(testMonster)
    def withHealth(hp: Int)                      = _health.set(hp)(testMonster)
    def withMaxHealth(hp: Int)                   = _maxHealth.set(hp)(testMonster)
    def withStrength(strScore: Stat)             = strengthLens.set(strScore)(testMonster)
    def withDexterity(dexScore: Stat)            = dexterityLens.set(dexScore)(testMonster)
    def withConstitution(conScore: Stat)         = constitutionLens.set(conScore)(testMonster)
    def withWisdom(wisScore: Stat)               = wisdomLens.set(wisScore)(testMonster)
    def withIntelligence(intScore: Stat)         = intelligenceLens.set(intScore)(testMonster)
    def withCharisma(chaScore: Stat)             = charismaLens.set(chaScore)(testMonster)
    def withBaseWeapon(weapon: Weapon)           = _baseWeapon.set(weapon)(testMonster)
    def withArmourClass(ac: Int)                 = _armourClass.set(ac)(testMonster)
    def withNoArmour()                           = _armour.set(NoArmour)(testMonster)
    def withNoOffHand()                          = _offHand.set(none[Equipment])(testMonster)
    def withResistance(creatureRes: DamageType*) = _resistances.set(creatureRes.toList)(testMonster)
    def withImmunity(creatureImm: DamageType*)   = _immunities.set(creatureImm.toList)(testMonster)
    def withNoResistances()                      = _resistances.set(List.empty)(testMonster)
    def withNoImmunities()                       = _immunities.set(List.empty)(testMonster)
    def withNoResistancesOrImmunities()          = testMonster.withNoResistances().withNoImmunities()

    def withAbilities(ablts: List[CombatantAbility]) = _abilities.set(ablts)(testMonster)

    def withCombatIndex(index: Int) = Combatant(index, testMonster)
  }

  implicit class CreatureOps(val creature: Creature) extends AnyVal {
    import Creature._

    def withHealth(hp: Int)                          = creatureHealthLens.set(hp)(creature)
    def withMaxHealth(hp: Int)                       = creatureMaxHealthLens.set(hp)(creature)
    def withStats(baseStats: BaseStats)              = creatureStatsLens.set(baseStats)(creature)
    def withBaseWeapon(baseWeapon: Weapon)           = creatureBaseWeaponLens.set(baseWeapon)(creature)
    def withArmour(armour: Armour)                   = creatureArmourLens.set(armour)(creature)
    def withOffHand(offHand: Equipment)              = creatureOffHandLens.set(offHand.some)(creature)
    def withArmourClass(ac: Int)                     = creatureArmourClassOptional.set(ac)(creature)
    def withAbilities(ablts: List[CombatantAbility]) = creatureAbilitiesLens.set(ablts)(creature)
    def withNoAbilities()                            = creatureAbilitiesLens.set(List.empty)(creature)

    def withProficiencyBonus(proficiencyBonus: ProficiencyBonus) =
      creatureProficiencyBonusOptional.set(proficiencyBonus)(creature)

    def withStrength(strengthScore: Stat) = creatureStrengthLens.set(strengthScore)(creature)
    def withDexterity(dexScore: Stat)     = creatureDexterityLens.set(dexScore)(creature)
    def withConstitution(conScore: Stat)  = creatureConstitutionLens.set(conScore)(creature)
    def withWisdom(wisScore: Stat)        = creatureConstitutionLens.set(wisScore)(creature)
    def withIntelligence(intScore: Stat)  = creatureIntelligenceLens.set(intScore)(creature)
    def withCharisma(chaScore: Stat)      = creatureCharismaLens.set(chaScore)(creature)

    def withNoArmour()  = creatureArmourLens.set(NoArmour)(creature)
    def withNoOffHand() = creatureOffHandLens.set(none[Equipment])(creature)

    def withResistance(creatureRes: DamageType*) = creatureResistancesLens.set(creatureRes.toList)(creature)
    def withImmunity(creatureImm: DamageType*)   = creatureImmunitiesLens.set(creatureImm.toList)(creature)
    def withNoResistances()                      = creatureResistancesLens.set(List.empty)(creature)
    def withNoImmunities()                       = creatureImmunitiesLens.set(List.empty)(creature)
    def withNoResistancesOrImmunities()          = creature.withNoResistances().withNoImmunities()

    def withLevel(level: Level)     = creatureLevelOptional.set(level)(creature)
    def withCombatIndex(index: Int) = Combatant(index, creature)
  }

  implicit class FighterOps(val fighter: Fighter) extends AnyVal {
    import Fighter._

    def withFightingStyle(fightingStyle: FighterFightingStyle) = _fightingStyles.set(List(fightingStyle))(fighter)
    def withAllAbilitiesUnused()                               = _abilityUsages.set(BaseFighterAbilities(false, false))(fighter)
    def withAllAbilitiesUsed()                                 = _abilityUsages.set(BaseFighterAbilities(true, true))(fighter)

    def withBonusActionUsed() = _bonusActionUsed.set(true)(fighter)
  }

  implicit class BattleMasterOps(val battleMaster: BattleMaster) extends AnyVal {
    import BattleMaster._

    def withSuperiorityDiceCount(count: Int) = _superiorityDiceCount.set(count)(battleMaster)
    def withAllAbilitiesUsed()               = _abilityUsages.set(BaseFighterAbilities(true, true))(battleMaster)
  }

  implicit class EldritchKnightOps(val eldritchKnight: EldritchKnight) extends AnyVal {
    import EldritchKnight._

    def withFightingStyle(fightingStyle: FighterFightingStyle) =
      _fightingStyles.set(List(fightingStyle))(eldritchKnight)
    def withAllAbilitiesUnused()          = _abilityUsages.set(BaseFighterAbilities(false, false))(eldritchKnight)
    def withAllBaseFighterAbilitiesUsed() = _abilityUsages.set(BaseFighterAbilities(true, true))(eldritchKnight)

    def withSpell(spell: Spell)      = _spellsKnown.set(Map(spell.spellLevel -> spell))(eldritchKnight)
    def withAllSpellSlotsAvailable() = _spellSlots.set(EldritchKnightSpellSlots(FirstLevelSpellSlot(2)))(eldritchKnight)
    def withNoSpellSlotsAvailable()  = _spellSlots.set(EldritchKnightSpellSlots(FirstLevelSpellSlot(0)))(eldritchKnight)
  }
}

trait TestData extends RandomDataGenerator {

  implicit val arbProficiencyBonus: Arbitrary[ProficiencyBonus] =
    Arbitrary {
      Gen
        .choose(0, 6)
        .map(refined.refineV[Interval.ClosedOpen[W.`0`.T, W.`7`.T]](_))
        .flatMap {
          case Right(i) => Gen.const(i)
          case Left(_)  => Gen.fail
        }
    }

  implicit val arbFirstLevelSpellSlot: Arbitrary[FirstLevelSpellSlot] = Arbitrary {
    for {
      // Currently on the Eldritch Knight has spell slots between 2 and 3 up to level five.
      count <- Gen.choose(2, 3)
    } yield FirstLevelSpellSlot(count)
  }

  implicit val arbEldritchKnightSpellSlots: Arbitrary[EldritchKnightSpellSlots] = Arbitrary {
    for {
      firstLevelSpellSlots <- arbFirstLevelSpellSlot.arbitrary
    } yield EldritchKnightSpellSlots(firstLevelSpellSlots)
  }

  implicit val arbStat: Arbitrary[Stat] =
    Arbitrary {
      Gen
        .choose(1, 30)
        .map(refined.refineV[Interval.ClosedOpen[W.`1`.T, W.`31`.T]](_))
        .flatMap {
          case Right(i) => Gen.const(i)
          case Left(_)  => Gen.fail
        }
    }

  implicit val arbBaseStats: Arbitrary[BaseStats] = cachedImplicit

  implicit val arbDamageType: Arbitrary[DamageType] = Arbitrary {
    Gen.oneOf(Bludgeoning, Piercing, Slashing, Magical)
  }

  implicit val arbWeaponType: Arbitrary[WeaponType] = Arbitrary {
    Gen.oneOf(Melee, Ranged)
  }

  implicit val arbLevel: Arbitrary[Level] = Arbitrary {
    Gen.oneOf(LevelOne, LevelTwo, LevelThree, LevelFour, LevelFive)
  }

  implicit val arbWeapon: Arbitrary[Weapon] = Arbitrary {
    for {
      weaponName       <- Gen.alphaStr.filter(_.nonEmpty)
      wpnType          <- arbWeaponType.arbitrary
      weaponDamageType <- arbDamageType.arbitrary
      twoHands         <- Gen.oneOf(true, false)
      wpnHitBonus      <- Gen.choose(0, 3)
      sides            <- Gen.choose(1, 12)
    } yield
      new Weapon {
        val name: String = weaponName
        val weaponType   = wpnType
        val damageType   = weaponDamageType
        val twoHanded    = twoHands

        override val hitBonus: Int = wpnHitBonus

        def damage(implicit rollStrategy: RollStrategy): Int = Dice.defaultRandomiser(sides)

      }
  }

  implicit val arbArmour: Arbitrary[Armour] = Arbitrary {
    for {
      armourName <- Gen.alphaStr
      baseArmour <- Gen.choose(5, 14)
    } yield
      new Armour {
        val name: String = armourName

        def armourClass(dexterity: Stat): Int = baseArmour + Modifier.mod(dexterity)
      }
  }

  implicit val arbShield: Arbitrary[Option[Shield]] = Arbitrary {
    Gen.oneOf(none[Shield], Shield().some) // TODO Gen any type of equipment
  }

  implicit val arbPlayer: Arbitrary[Player] = Arbitrary {
    for {
      lvl <- arbLevel.arbitrary
    } yield
      new Player {
        val level: Level             = lvl
        val bonusActionUsed: Boolean = false
      }
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      n         <- Gen.alphaStr
      hp        <- Gen.choose(10, 80)
      baseStats <- arbBaseStats.arbitrary
      wpn       <- arbWeapon.arbitrary
      armr      <- arbArmour.arbitrary
      optShield <- arbShield.arbitrary
      cType     <- Gen.oneOf(PlayerCharacter, Monster)
      profBonus <- arbProficiencyBonus.arbitrary
    } yield
      new Creature {
        val creatureType: CreatureType = cType
        val health: Int                = hp
        val maxHealth: Int             = hp
        val stats: BaseStats           = baseStats

        val baseWeapon: Weapon    = wpn
        def weapon[_: RS]: Weapon = wpn

        val armour: Armour = armr

        val offHand: Option[Equipment] = optShield

        val armourClass: Int = armour.armourClass(stats.dexterity)

        val proficiencyBonus: ProficiencyBonus = profBonus

        val resistances: List[DamageType]     = List.empty
        val immunities: List[DamageType]      = List.empty
        val bonusActionUsed: Boolean          = false
        val name: String                      = n
        val abilities: List[CombatantAbility] = standardCoreAbilities

        def updateHealth(modification: Int): Creature =
          throw new NotImplementedError("Impossible to implement, results in recursive definition of Creature")

        def scoresCritical(roll: Int): Boolean = roll == 20
      }
  }

  implicit val arbGoblin: Arbitrary[Goblin] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      Goblin(
        creature.health,
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        creature.resistances,
        creature.immunities,
        List.empty, // TODO add core abilities?
        creature.name
      )
  }

  implicit val arbTestMonster: Arbitrary[TestMonster] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      TestMonster(
        creature.health,
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        0,
        creature.resistances,
        creature.immunities,
        List.empty, // TODO add core abilities?
        creature.name
      )
  }

  implicit val arbFighterFightingStyle: Arbitrary[Seq[FighterFightingStyle]] = Arbitrary {
    Gen.someOf(Archery, Defense, Dueling, GreatWeaponFighting, Protection, TwoWeaponFighting)
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      player         <- arbPlayer.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Fighter(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        fightingStyles.toList,
        BaseFighterAbilities.allUnused(),
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        player.bonusActionUsed,
        Fighter.standardFighterAbilities,
        creature.name
      )
  }

  implicit val arbChampion: Arbitrary[Champion] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      player         <- arbPlayer.arbitrary
      armour         <- arbArmour.arbitrary
      shield         <- arbShield.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Champion(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.baseWeapon,
        armour,
        shield,
        fightingStyles.toList,
        BaseFighterAbilities.allUnused(),
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        player.bonusActionUsed,
        Champion.standardChampionAbilities,
        creature.name
      )
  }

  implicit val arbBattleMaster: Arbitrary[BattleMaster] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      player         <- arbPlayer.arbitrary
      armour         <- arbArmour.arbitrary
      shield         <- arbShield.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      BattleMaster(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.baseWeapon,
        armour,
        shield,
        fightingStyles.toList,
        BaseFighterAbilities.allUnused(),
        superiorityDiceCount = 4,
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        player.bonusActionUsed,
        BattleMaster.standardBattleMasterAbilities,
        creature.name
      )
  }

  implicit val arbEldritchKnight: Arbitrary[EldritchKnight] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      player         <- arbPlayer.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
      spellSlots     <- arbEldritchKnightSpellSlots.arbitrary
    } yield
      EldritchKnight(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        fightingStyles.toList,
        BaseFighterAbilities.allUnused(),
        creature.proficiencyBonus,
        Map(ChromaticOrb.spellLevel -> ChromaticOrb), // TODO randomise spells
        spellSlots,
        creature.resistances,
        creature.immunities,
        player.bonusActionUsed,
        EldritchKnight.standardEldritchKnightAbilities,
        creature.name
      )
  }
}
