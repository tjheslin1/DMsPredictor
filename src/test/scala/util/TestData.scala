package util

import cats.data.NonEmptyList
import cats.syntax.option._
import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.standardCoreAbilities
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger.rangerSpellSlots
import io.github.tjheslin1.dmspredictor.classes.ranger._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.classes.{fighter, ranger, Player}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.{SpellLevel, _}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Legendary, Monster, Werewolf, Zombie}
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

    def withName(creatureName: String)   = _name.set(creatureName)(testMonster)
    def withHealth(hp: Int)              = _health.set(hp)(testMonster)
    def withMaxHealth(hp: Int)           = _maxHealth.set(hp)(testMonster)
    def withStrength(strScore: Stat)     = strengthLens.set(strScore)(testMonster)
    def withDexterity(dexScore: Stat)    = dexterityLens.set(dexScore)(testMonster)
    def withConstitution(conScore: Stat) = constitutionLens.set(conScore)(testMonster)
    def withWisdom(wisScore: Stat)       = wisdomLens.set(wisScore)(testMonster)
    def withIntelligence(intScore: Stat) = intelligenceLens.set(intScore)(testMonster)
    def withCharisma(chaScore: Stat)     = charismaLens.set(chaScore)(testMonster)
    def withBaseWeapon(weapon: Weapon)   = _baseWeapon.set(weapon)(testMonster)
    def withArmourClass(ac: Int)         = _armourClass.set(ac)(testMonster)
    def withNoArmour()                   = _armour.set(NoArmour)(testMonster)
    def withNoOffHand()                  = _offHand.set(none[Equipment])(testMonster)
    def withDamageResistance(creatureRes: DamageType*) =
      _damageResistances.set(creatureRes.toList)(testMonster)
    def withDamageImmunity(creatureImm: DamageType*) =
      _damageImmunities.set(creatureImm.toList)(testMonster)
    def withConditionResistance(creatureRes: ConditionType*) =
      _conditionResistances.set(creatureRes.toList)(testMonster)
    def withConditionImmunity(creatureImm: ConditionType*) =
      _conditionImmunities.set(creatureImm.toList)(testMonster)
    def withNoResistances()             = _damageResistances.set(List.empty)(testMonster)
    def withNoImmunities()              = _damageImmunities.set(List.empty)(testMonster)
    def withNoResistancesOrImmunities() = testMonster.withNoResistances().withNoImmunities()

    def withAbilities(ablts: List[CombatantAbility]) = _abilities.set(ablts)(testMonster)

    def withCreatureType(creatureType: CreatureType) = _creatureType.set(creatureType)(testMonster)
    def withChallengeRating(cr: Double)              = _challengeRating.set(cr)(testMonster)

    def withSavingThrowScores(strength: Int = 0,
                              dexterity: Int = 0,
                              constitution: Int = 0,
                              wisdom: Int = 0,
                              intelligence: Int = 0,
                              charisma: Int = 0) = {
      val savingThrowScores = Map(
        Strength     -> strength,
        Dexterity    -> dexterity,
        Constitution -> constitution,
        Wisdom       -> wisdom,
        Intelligence -> intelligence,
        Charisma     -> charisma
      )

      _savingThrowScores.set(savingThrowScores)(testMonster)
    }

    def withStrengthSavingThrowScore(strScore: Int) = {
      val savingThrowScores = testMonster.savingThrowScores.map {
        case (Strength, _)      => Strength  -> strScore
        case (attribute, score) => attribute -> score
      }

      _savingThrowScores.set(savingThrowScores)(testMonster)
    }

    def withDexteritySavingThrowScore(dexScore: Int) = {
      val savingThrowScores = testMonster.savingThrowScores.map {
        case (Dexterity, _)     => Dexterity -> dexScore
        case (attribute, score) => attribute -> score
      }

      _savingThrowScores.set(savingThrowScores)(testMonster)
    }

    def withCombatIndex(index: Int) = Combatant(index, testMonster)
  }

  implicit class TestSpellCastingMonsterOps(val testSpellCastingMonster: TestSpellCastingMonster)
      extends AnyVal {
    import TestSpellCastingMonster._

    def withName(creatureName: String)   = _name.set(creatureName)(testSpellCastingMonster)
    def withHealth(hp: Int)              = _health.set(hp)(testSpellCastingMonster)
    def withMaxHealth(hp: Int)           = _maxHealth.set(hp)(testSpellCastingMonster)
    def withStrength(strScore: Stat)     = strengthLens.set(strScore)(testSpellCastingMonster)
    def withDexterity(dexScore: Stat)    = dexterityLens.set(dexScore)(testSpellCastingMonster)
    def withConstitution(conScore: Stat) = constitutionLens.set(conScore)(testSpellCastingMonster)
    def withWisdom(wisScore: Stat)       = wisdomLens.set(wisScore)(testSpellCastingMonster)
    def withIntelligence(intScore: Stat) = intelligenceLens.set(intScore)(testSpellCastingMonster)
    def withCharisma(chaScore: Stat)     = charismaLens.set(chaScore)(testSpellCastingMonster)
    def withBaseWeapon(weapon: Weapon)   = _baseWeapon.set(weapon)(testSpellCastingMonster)
    def withArmourClass(ac: Int)         = _armourClass.set(ac)(testSpellCastingMonster)
    def withNoArmour()                   = _armour.set(NoArmour)(testSpellCastingMonster)
    def withNoOffHand()                  = _offHand.set(none[Equipment])(testSpellCastingMonster)

    def withResistance(creatureRes: DamageType*) =
      _damageResistances.set(creatureRes.toList)(testSpellCastingMonster)

    def withImmunity(creatureImm: DamageType*) =
      _damageImmunities.set(creatureImm.toList)(testSpellCastingMonster)

    def withNoResistances() = _damageResistances.set(List.empty)(testSpellCastingMonster)
    def withNoImmunities()  = _damageImmunities.set(List.empty)(testSpellCastingMonster)
    def withNoResistancesOrImmunities() =
      testSpellCastingMonster.withNoResistances().withNoImmunities()

    def withAbilities(ablts: List[CombatantAbility]) =
      _abilities.set(ablts)(testSpellCastingMonster)

    def withCreatureType(creatureType: CreatureType) =
      _creatureType.set(creatureType)(testSpellCastingMonster)
    def withChallengeRating(cr: Double) = _challengeRating.set(cr)(testSpellCastingMonster)

    def withSpellSlots(spellSlots: SpellSlots) =
      _spellSlots.set(spellSlots)(testSpellCastingMonster)

    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(testSpellCastingMonster)

    def withSpellsKnown(spells: Spell*) =
      _spellsKnown.set(spells.map(spell => (spell.spellLevel, spell.spellEffect) -> spell).toMap)(
        testSpellCastingMonster)

    def withConcentratingOn(concentrationSpell: Option[Spell]) =
      _concentratingSpell.set(concentrationSpell)(testSpellCastingMonster)
  }

  implicit class MonsterOps(val monster: Monster) extends AnyVal {
    import Monster._

    def withArmourClass(ac: Int) = monsterArmourClassLens.set(ac)(monster)
  }

  implicit class LegendaryOps(val legendaryCreature: Legendary) extends AnyVal {
    import Legendary._

    def withNoLegendaryResistancesLeft() =
      legendaryResistancesLens.set(0)(legendaryCreature)

    def withLegendaryResistanceCount(count: Int) =
      legendaryResistancesLens.set(count)(legendaryCreature)
  }

  implicit class CreatureOps(val creature: Creature) extends AnyVal {
    import Creature._

    def withHealth(hp: Int)                          = creatureHealthLens.set(hp)(creature)
    def withMaxHealth(hp: Int)                       = creatureMaxHealthLens.set(hp)(creature)
    def withStats(baseStats: BaseStats)              = creatureStatsLens.set(baseStats)(creature)
    def withBaseWeapon(baseWeapon: Weapon)           = creatureBaseWeaponLens.set(baseWeapon)(creature)
    def withArmour(armour: Armour)                   = creatureArmourLens.set(armour)(creature)
    def withOffHand(offHand: Equipment)              = creatureOffHandLens.set(offHand.some)(creature)
    def withAbilities(ablts: List[CombatantAbility]) = creatureAbilitiesLens.set(ablts)(creature)
    def withNoAbilities()                            = creatureAbilitiesLens.set(List.empty)(creature)

    def withStrength(strengthScore: Stat) = creatureStrengthLens.set(strengthScore)(creature)
    def withDexterity(dexScore: Stat)     = creatureDexterityLens.set(dexScore)(creature)
    def withConstitution(conScore: Stat)  = creatureConstitutionLens.set(conScore)(creature)
    def withWisdom(wisScore: Stat)        = creatureWisdomLens.set(wisScore)(creature)
    def withIntelligence(intScore: Stat)  = creatureIntelligenceLens.set(intScore)(creature)
    def withCharisma(chaScore: Stat)      = creatureCharismaLens.set(chaScore)(creature)

    def withNoArmour()  = creatureArmourLens.set(NoArmour)(creature)
    def withNoOffHand() = creatureOffHandLens.set(none[Equipment])(creature)

    def withDamageResistance(creatureRes: DamageType*) =
      creatureDamageResistancesLens.set(creatureRes.toList)(creature)
    def withDamageImmunity(creatureImm: DamageType*) =
      creatureDamageImmunitiesLens.set(creatureImm.toList)(creature)

    def withConditionResistance(creatureRes: ConditionType*) =
      creatureConditionResistancesLens.set(creatureRes.toList)(creature)
    def withConditionImmunity(creatureImm: ConditionType*) =
      creatureConditionImmunitiesLens.set(creatureImm.toList)(creature)

    def withNoDamageResistances() = creatureDamageResistancesLens.set(List.empty)(creature)
    def withNoDamageImmunities()  = creatureDamageImmunitiesLens.set(List.empty)(creature)
    def withNoDamageResistancesOrImmunities() =
      creature.withNoDamageResistances().withNoDamageImmunities()

    def withCondition(condition: Condition) = creatureConditionsLens.set(List(condition))(creature)
    def withConditions(conditions: Condition*) =
      creatureConditionsLens.set(conditions.toList)(creature)
    def withNoConditions() = creatureConditionsLens.set(List.empty[Condition])(creature)

    def withReactionUsed(used: Boolean) = creatureReactionUsedLens.set(used)(creature)

    def withAttackStatus(attackStatus: AttackStatus) =
      creatureAttackStatusLens.set(attackStatus)(creature)
    def withDefenseStatus(defenseStatus: AttackStatus) =
      creatureDefenseStatusLens.set(defenseStatus)(creature)

    def withIsAlive(isAlive: Boolean) = creatureIsAliveLens.set(isAlive)(creature)

    def withCombatIndex(index: Int) = Combatant(index, creature)

    def withSkills(perception: Int, stealth: Int) =
      creatureSkillsOptional.set(Skills(perception, stealth))(creature)
  }

  implicit class PlayerOps(val player: Player) extends AnyVal {
    import Player._

    def withLevel(level: Level) = playerLevelLens.set(level)(player)

    def withProficiencyBonus(proficiencyBonus: ProficiencyBonus) =
      playerProficiencyBonusLens.set(proficiencyBonus)(player)
  }

  implicit class FighterOps(val fighter: Fighter) extends AnyVal {
    import Fighter._

    def withFightingStyle(fightingStyle: FighterFightingStyle) =
      _fightingStyles.set(List(fightingStyle))(fighter)
    def withAllAbilitiesUnused() = _abilityUsages.set(BaseFighterAbilities(false, false))(fighter)
    def withAllAbilitiesUsed()   = _abilityUsages.set(BaseFighterAbilities(true, true))(fighter)

    def withBonusActionUsed() = _bonusActionUsed.set(true)(fighter)
  }

  implicit class ChampionOps(val champion: Champion) extends AnyVal {
    import Champion._

    def withAbilitiesUsed(secondWindUsed: Boolean, actionSurgeUsed: Boolean) =
      _abilityUsages.set(BaseFighterAbilities(secondWindUsed, actionSurgeUsed))(champion)
  }

  implicit class ClericOps(val cleric: Cleric) extends AnyVal {
    import Cleric._

    def withConcentratingOn(concentratingSpell: Spell) =
      _concentratingSpell.set(concentratingSpell.some)(cleric)

    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(cleric)
    def withSpellsKnown(spells: Spell*) =
      _spellsKnown.set(spells.map(spell => (spell.spellLevel, spell.spellEffect) -> spell).toMap)(
        cleric)
    def withAllSpellSlotsAvailableForLevel(level: Level) =
      _spellSlots.set(clericSpellSlots(level))(cleric)
    def withNoSpellSlotsAvailable() =
      _spellSlots.set(SpellSlots(0, 0, 0, 0, 0, 0, 0, 0, 0))(cleric)
    def withChannelDivinityUsed() = _channelDivinityUsed.set(true)(cleric)
  }

  implicit class BarbarianOps(val barbarian: Barbarian) extends AnyVal {
    import Barbarian._

    def withRageUsagesLeft(count: Int) = _rageUsages.set(count)(barbarian)
    def withRageTurnsLeft(count: Int)  = _rageTurnsLeft.set(count)(barbarian)
  }

  implicit class BerserkerOps(val berserker: Berserker) extends AnyVal {
    import Berserker._

    def withRageUsagesLeft(count: Int) = _rageUsages.set(count)(berserker)
    def withRageTurnsLeft(count: Int)  = _rageTurnsLeft.set(count)(berserker)
    def withInFrenzy()                 = _inFrenzy.set(true)(berserker)

    def withBonusActionUsed() = _bonusActionUsed.set(true)(berserker)
  }

  implicit class RogueOps(val rogue: Rogue) extends AnyVal {
    import Rogue._

    def withStealthScore(score: Int) =
      _skills.set(Skills(rogue.skills.perception, score))(rogue)
    def isHiddenFrom(enemies: List[Combatant]) = _hiddenFrom.set(enemies)(rogue)

    def withBonusActionUsed() = _bonusActionUsed.set(true)(rogue)
  }

  implicit class WizardOps(val wizard: Wizard) extends AnyVal {
    import Wizard._

    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(wizard)
    def withSpellsKnown(spells: Spell*) =
      _spellsKnown.set(spells.map(spell => (spell.spellLevel, spell.spellEffect) -> spell).toMap)(
        wizard)
    def withAllSpellSlotsAvailableForLevel(level: Level) =
      _spellSlots.set(wizardSpellSlots(level))(wizard)
    def withSpellSlots(spellSlots: SpellSlots) =
      _spellSlots.set(spellSlots)(wizard)

    def withNoSpellSlotsAvailable() =
      _spellSlots.set(SpellSlots(0, 0, 0, 0, 0, 0, 0, 0, 0))(wizard)

    def withConcentratingOn(concentrationSpell: Spell) =
      _concentratingSpell.set(concentrationSpell.some)(wizard)

    def withCastShieldOnReaction(willCast: Boolean) = _castShieldAsReaction.set(willCast)(wizard)
    def withMageArmourPrepared(prepared: Boolean)   = _mageArmourPrepared.set(prepared)(wizard)
  }

  implicit class RangerOps(val ranger: Ranger) extends AnyVal {
    import Ranger._

    def withFightingStyle(fightingStyle: RangerFightingStyle) =
      _fightingStyles.set(List(fightingStyle))(ranger)

    def withBonusActionUsed() = _bonusActionUsed.set(true)(ranger)

    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(ranger)

    def withAllSpellSlotsAvailableForLevel(level: Level) =
      _spellSlots.set(rangerSpellSlots(level))(ranger)

    def withConcentratingOn(spell: Spell) = _concentratingSpell.set(spell.some)(ranger)
  }

  implicit class HunterOps(val hunter: Hunter) extends AnyVal {
    import Hunter._

    def withColossusSlayerUsed(used: Boolean) = _colossusSlayerUsed.set(used)(hunter)

    def withFightingStyle(fightingStyle: RangerFightingStyle) =
      _fightingStyles.set(List(fightingStyle))(hunter)

    def withBonusActionUsed() = _bonusActionUsed.set(true)(hunter)

    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(hunter)

    def withAllSpellSlotsAvailableForLevel(level: Level) =
      _spellSlots.set(rangerSpellSlots(level))(hunter)

    def withConcentratingOn(spell: Spell) = _concentratingSpell.set(spell.some)(hunter)
  }

  implicit class LichOps(val lich: Lich) extends AnyVal {
    import Lich._

    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(lich)

    def withSpellsKnown(spells: Spell*) =
      _spellsKnown.set(spells.map(spell => (spell.spellLevel, spell.spellEffect) -> spell).toMap)(
        lich)

    def withSpellSlots(spellSlots: SpellSlots) =
      _spellSlots.set(spellSlots)(lich)

    def withNoSpellSlots() = _spellSlots.set(SpellSlots(0, 0, 0, 0, 0, 0, 0, 0, 0))(lich)
  }
}

trait TestData extends RandomDataGenerator {

  val nonEmptyString: Gen[String] = Gen.chooseNum(5, 15).flatMap { n =>
    Gen.buildableOfN[String, Char](n, Gen.alphaChar)
  }

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

  implicit val arbFirstLevelSpellSlot: Arbitrary[FirstLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.choose(1, 3)
    } yield FirstLevelSpellSlots(count)
  }

  implicit val arbSecondLevelSpellSlot: Arbitrary[SecondLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.choose(1, 3)
    } yield SecondLevelSpellSlots(count)
  }

  implicit val arbThirdLevelSpellSlot: Arbitrary[ThirdLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.choose(1, 3)
    } yield ThirdLevelSpellSlots(count)
  }

  implicit val arbFourthLevelSpellSlot: Arbitrary[FourthLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.choose(1, 3)
    } yield FourthLevelSpellSlots(count)
  }

  implicit val arbFifthLevelSpellSlot: Arbitrary[FifthLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.choose(1, 2)
    } yield FifthLevelSpellSlots(count)
  }

  implicit val arbSixthLevelSpellSlot: Arbitrary[SixthLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.const(1)
    } yield SixthLevelSpellSlots(count)
  }

  implicit val arbSeventhLevelSpellSlot: Arbitrary[SeventhLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.const(1)
    } yield SeventhLevelSpellSlots(count)
  }

  implicit val arbEighthLevelSpellSlot: Arbitrary[EighthLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.const(1)
    } yield EighthLevelSpellSlots(count)
  }

  implicit val arbNinthLevelSpellSlot: Arbitrary[NinthLevelSpellSlots] = Arbitrary {
    for {
      count <- Gen.const(1)
    } yield NinthLevelSpellSlots(count)
  }

  implicit val arbSpellSlots: Arbitrary[SpellSlots] = Arbitrary {
    for {
      firstLevelSpellSlots   <- arbFirstLevelSpellSlot.arbitrary
      secondLevelSpellSlots  <- arbSecondLevelSpellSlot.arbitrary
      thirdLevelSpellSlots   <- arbThirdLevelSpellSlot.arbitrary
      fourthLevelSpellSlots  <- arbFourthLevelSpellSlot.arbitrary
      fifthLevelSpellSlots   <- arbFifthLevelSpellSlot.arbitrary
      sixthLevelSpellSlots   <- arbSixthLevelSpellSlot.arbitrary
      seventhLevelSpellSlots <- arbSeventhLevelSpellSlot.arbitrary
      eighthLevelSpellSlots  <- arbEighthLevelSpellSlot.arbitrary
      ninthLevelSpellSlots   <- arbNinthLevelSpellSlot.arbitrary
    } yield
      SpellSlots(
        firstLevelSpellSlots,
        secondLevelSpellSlots,
        thirdLevelSpellSlots,
        fourthLevelSpellSlots,
        fifthLevelSpellSlots,
        sixthLevelSpellSlots,
        seventhLevelSpellSlots,
        eighthLevelSpellSlots,
        ninthLevelSpellSlots
      )
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
      weaponName       <- nonEmptyString
      wpnType          <- arbWeaponType.arbitrary
      weaponDamageType <- arbDamageType.arbitrary
      isTwoHanded      <- Gen.oneOf(true, false)
      isFinesse        <- Gen.oneOf(true, false)
      wpnHitBonus      <- Gen.choose(0, 3)
      sides            <- Gen.choose(1, 12)
    } yield
      new Weapon {
        val name: String = weaponName
        val weaponType   = wpnType
        val damageType   = weaponDamageType
        val twoHanded    = isTwoHanded
        val finesse      = isFinesse

        override val hitBonus: Int = wpnHitBonus

        def damage(implicit rollStrategy: RollStrategy): Int = Dice.defaultRandomiser(sides)

      }
  }

  implicit val arbArmour: Arbitrary[Armour] = Arbitrary {
    for {
      armourName <- nonEmptyString
      baseArmour <- Gen.choose(5, 14)
    } yield
      new Armour {
        val name: String = armourName

        def armourClass(dexterity: Stat): Int = baseArmour + Modifier.mod(dexterity)
      }
  }

  implicit val arbShield: Arbitrary[Option[Equipment]] = Arbitrary {
    Gen.oneOf(none[Equipment], Shield.some) // TODO Gen any type of equipment
  }

  implicit val arbChallengeRating: Arbitrary[Double] = Arbitrary {
    Gen.oneOf(0.25, 0.5, 1, 2, 3, 5)
  }

  implicit val arbSkills: Arbitrary[Skills] = Arbitrary {
    for {
      perception <- Gen.choose(0, 6)
      stealth    <- Gen.choose(0, 6)
    } yield Skills(perception, stealth)
  }

  implicit val arbMonsterType: Arbitrary[CreatureType] = Arbitrary {
    Gen.oneOf(Undead, Humanoid)
  }

  implicit val arbSavingThrowProficiencies: Arbitrary[List[Attribute]] = Arbitrary {
    val attributes = List(Strength, Dexterity, Constitution, Wisdom, Intelligence, Charisma)
    Gen.pick(2, attributes).map(_.toList)
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      n              <- nonEmptyString
      hp             <- Gen.choose(10, 80)
      baseStats      <- arbBaseStats.arbitrary
      wpn            <- arbWeapon.arbitrary
      armr           <- arbArmour.arbitrary
      optShield      <- arbShield.arbitrary
      creatureSkills <- arbSkills.arbitrary
    } yield
      new Creature {
        val creatureType: CreatureType = PlayerCharacter
        val health: Int                = hp
        val maxHealth: Int             = hp
        val stats: BaseStats           = baseStats

        val baseWeapon: Weapon    = wpn
        def weapon[_: RS]: Weapon = wpn

        val armour: Armour = armr

        val offHand: Option[Equipment] = optShield

        val armourClass: Int = armour.armourClass(baseStats.dexterity)

        val damageVulnerabilities: List[DamageType]   = List.empty[DamageType]
        val damageResistances: List[DamageType]       = List.empty[DamageType]
        val damageImmunities: List[DamageType]        = List.empty[DamageType]
        val conditionResistances: List[ConditionType] = List.empty[ConditionType]
        val conditionImmunities: List[ConditionType]  = List.empty[ConditionType]
        val bonusActionUsed: Boolean                  = false
        val reactionUsed: Boolean                     = false
        val name: String                              = n
        val abilities: List[CombatantAbility]         = standardCoreAbilities
        val conditions: List[Condition]               = List.empty
        val attackStatus: AttackStatus                = Regular
        val defenseStatus: AttackStatus               = Regular

        val skills: Skills = creatureSkills

        val reactionOnHit: Option[OnHitReaction]       = None
        val reactionOnDamage: Option[OnDamageReaction] = None

        val isAlive: Boolean = true

        def scoresCritical(roll: Int): Boolean = roll == 20

        def updateHealth[_: RS](dmg: Int,
                                damageType: DamageType,
                                attackResult: AttackResult): Creature =
          throw new NotImplementedError(
            "Impossible to implement, results in recursive definition of Creature")

        def resetStartOfTurn(): Creature =
          throw new NotImplementedError(
            "Random generation should delegate to specific resetStartOfTurn()")
      }
  }

  implicit val arbPlayer: Arbitrary[Player] = Arbitrary {
    for {
      lvl              <- arbLevel.arbitrary
      creature         <- arbCreature.arbitrary
      profBonus        <- arbProficiencyBonus.arbitrary
      savingThrowProfs <- arbSavingThrowProficiencies.arbitrary
    } yield
      new Player {
        val level: Level             = lvl
        val bonusActionUsed: Boolean = false
        val reactionUsed: Boolean    = false

        val health: Int        = creature.health
        val maxHealth: Int     = creature.maxHealth
        val stats: BaseStats   = creature.stats
        val baseWeapon: Weapon = creature.baseWeapon

        val savingThrowProficiencies: NonEmptyList[Attribute] =
          NonEmptyList.fromListUnsafe(savingThrowProfs)

        def weapon[_: RS]: Weapon = creature.weapon

        val armour: Armour                            = creature.armour
        val offHand: Option[Equipment]                = creature.offHand
        val armourClass: Int                          = creature.armourClass
        val proficiencyBonus: ProficiencyBonus        = profBonus
        val damageVulnerabilities: List[DamageType]   = creature.damageResistances
        val damageResistances: List[DamageType]       = creature.damageResistances
        val damageImmunities: List[DamageType]        = creature.damageImmunities
        val conditionResistances: List[ConditionType] = creature.conditionResistances
        val conditionImmunities: List[ConditionType]  = creature.conditionImmunities
        val name: String                              = creature.name
        val abilities: List[CombatantAbility]         = creature.abilities
        val conditions: List[Condition]               = List.empty
        val attackStatus: AttackStatus                = creature.attackStatus
        val defenseStatus: AttackStatus               = creature.defenseStatus

        val skills: Skills = creature.skills

        val isAlive: Boolean = creature.isAlive

        val reactionOnHit: Option[OnHitReaction]       = creature.reactionOnHit
        val reactionOnDamage: Option[OnDamageReaction] = creature.reactionOnDamage

        def updateHealth[_: RS](dmg: Int,
                                damageType: DamageType,
                                attackResult: AttackResult): Creature =
          throw new NotImplementedError(
            "Random generation should delegate to specific updateHealth()")

        def scoresCritical(roll: Int): Boolean = creature.scoresCritical(roll)

        def resetStartOfTurn(): Creature =
          throw new NotImplementedError(
            "Random generation should delegate to specific resetStartOfTurn()")
      }
  }

  implicit val arbGoblin: Arbitrary[Goblin] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      Goblin(
        creature.health,
        creature.health,
        name = creature.name
      )
  }

  implicit val arbLich: Arbitrary[Lich] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      Lich(
        creature.health,
        creature.health,
        name = creature.name
      )
  }

  implicit val arbVampire: Arbitrary[Vampire] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      Vampire(
        creature.health,
        creature.health,
        name = creature.name
      )
  }

  implicit val arbWerewolf: Arbitrary[Werewolf] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      Werewolf(
        creature.health,
        creature.health,
        name = creature.name
      )
  }

  implicit val arbZombie: Arbitrary[Zombie] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      Zombie(
        creature.health,
        creature.health,
        name = creature.name
      )
  }

  implicit val arbTestMonster: Arbitrary[TestMonster] = Arbitrary {
    for {
      creature     <- arbCreature.arbitrary
      creatureType <- arbMonsterType.arbitrary
      cr           <- arbChallengeRating.arbitrary
      arbSkills    <- arbSkills.arbitrary
    } yield
      TestMonster(
        creature.health,
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        creature.damageVulnerabilities,
        creature.damageResistances,
        creature.damageImmunities,
        creature.conditionResistances,
        creature.conditionImmunities,
        List.empty[CombatantAbility], // TODO add core abilities?
        creature.conditions,
        reactionUsed = false,
        creature.attackStatus,
        creature.defenseStatus,
        creatureType,
        cr,
        arbSkills.perception,
        arbSkills.stealth,
        TestMonster.defaultScores,
        creature.isAlive,
        creature.name
      )
  }

  implicit val arbTestSpellCastingMonster: Arbitrary[TestSpellCastingMonster] = Arbitrary {
    for {
      creature     <- arbCreature.arbitrary
      creatureType <- arbMonsterType.arbitrary
      cr           <- arbChallengeRating.arbitrary
      arbSkills    <- arbSkills.arbitrary
    } yield
      TestSpellCastingMonster(
        creature.health,
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        creature.damageVulnerabilities,
        creature.damageResistances,
        creature.damageImmunities,
        creature.conditionResistances,
        creature.conditionImmunities,
        List.empty[CombatantAbility], // TODO add core abilities?
        creature.conditions,
        reactionUsed = false,
        creature.attackStatus,
        creature.defenseStatus,
        creatureType,
        cr,
        arbSkills.perception,
        arbSkills.stealth,
        TestSpellCastingMonster.defaultScores,
        Map.empty[(SpellLevel, spellcasting.SpellEffect), Spell],
        SpellSlots(0, 0, 0),
        none[Spell],
        0,
        LevelOne,
        creature.isAlive,
        creature.name
      )
  }

  implicit val arbFighterFightingStyle: Arbitrary[Seq[FighterFightingStyle]] = Arbitrary {
    Gen.someOf(fighter.Archery,
               fighter.Defense,
               fighter.Dueling,
               GreatWeaponFighting,
               Protection,
               fighter.TwoWeaponFighting)
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      player         <- arbPlayer.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Fighter(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        player.armour,
        player.offHand,
        fightingStyles.toList,
        BaseFighterAbilities.allUnused,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Fighter.standardFighterAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        player.isAlive,
        player.name
      )
  }

  implicit val arbChampion: Arbitrary[Champion] = Arbitrary {
    for {
      player         <- arbPlayer.arbitrary
      armour         <- arbArmour.arbitrary
      shield         <- arbShield.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Champion(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        armour,
        shield,
        fightingStyles.toList,
        BaseFighterAbilities.allUnused,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Champion.standardChampionAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        player.isAlive,
        player.name
      )
  }

  implicit val arbBarbarian: Arbitrary[Barbarian] = Arbitrary {
    for {
      player <- arbPlayer.arbitrary
      level  <- arbLevel.arbitrary
    } yield
      Barbarian(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        BaseBarbarian.rageUsagesPerLevel(level),
        player.skills,
        player.armour,
        player.offHand,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Barbarian.standardBarbarianAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        false,
        10,
        player.isAlive,
        player.name
      )
  }

  implicit val arbBerserker: Arbitrary[Berserker] = Arbitrary {
    for {
      player <- arbPlayer.arbitrary
      level  <- arbLevel.arbitrary
    } yield
      Berserker(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        BaseBarbarian.rageUsagesPerLevel(level),
        player.skills,
        player.armour,
        player.offHand,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Barbarian.standardBarbarianAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        false,
        false,
        10,
        player.isAlive,
        player.name
      )
  }

  implicit val arbCleric: Arbitrary[Cleric] = Arbitrary {
    for {
      player <- arbPlayer.arbitrary
      level  <- arbLevel.arbitrary
    } yield
      Cleric(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        Cleric.clericSpellSlots(level),
        Cleric.standardClericSpellList,
        channelDivinityUsed = false,
        player.armour,
        player.offHand,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        bonusActionUsed = player.bonusActionUsed,
        reactionUsed = player.reactionUsed,
        Cleric.standardClericAbilities,
        player.conditions,
        attackStatus = player.attackStatus,
        defenseStatus = player.defenseStatus,
        concentratingSpell = None,
        player.isAlive,
        player.name
      )
  }

  implicit val arbRogue: Arbitrary[Rogue] = Arbitrary {
    for {
      player <- arbPlayer.arbitrary
      level  <- arbLevel.arbitrary
    } yield
      Rogue(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        player.armour,
        none[Equipment],
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Rogue.standardRogueAbilities,
        List.empty[Combatant],
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        player.isAlive,
        player.name
      )
  }

  implicit val arbWizard: Arbitrary[Wizard] = Arbitrary {
    for {
      player <- arbPlayer.arbitrary
      level  <- arbLevel.arbitrary
    } yield
      Wizard(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        Wizard.wizardSpellSlots(player.level),
        Wizard.standardWizardSpellList,
        castShieldAsReaction = true,
        mageArmourPrepared = true,
        NoArmour,
        none[Equipment],
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Wizard.standardWizardAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        concentratingSpell = none[Spell],
        player.isAlive,
        player.name
      )
  }

  implicit val arbRangerFightingStyle: Arbitrary[Seq[RangerFightingStyle]] = Arbitrary {
    Gen.someOf(ranger.Archery, ranger.Defense, ranger.Dueling, ranger.TwoWeaponFighting)
  }

  implicit val arbRanger: Arbitrary[Ranger] = Arbitrary {
    for {
      player         <- arbPlayer.arbitrary
      fightingStyles <- arbRangerFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Ranger(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        rangerSpellSlots(level),
        Ranger.standardRangerSpellList,
        player.armour,
        player.offHand,
        fightingStyles.toList,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Ranger.standardRangerAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        concentratingSpell = none[Spell],
        player.isAlive,
        player.name
      )
  }

  implicit val arbHunter: Arbitrary[Hunter] = Arbitrary {
    for {
      player         <- arbPlayer.arbitrary
      fightingStyles <- arbRangerFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Hunter(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        rangerSpellSlots(level),
        Hunter.standardHunterSpellList,
        player.armour,
        player.offHand,
        fightingStyles.toList,
        player.proficiencyBonus,
        player.damageVulnerabilities,
        player.damageResistances,
        player.damageImmunities,
        player.conditionResistances,
        player.conditionImmunities,
        player.bonusActionUsed,
        player.reactionUsed,
        colossusSlayerUsed = false,
        Hunter.standardHunterAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
        none[Spell],
        player.isAlive,
        player.name
      )
  }
}
