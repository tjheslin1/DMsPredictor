package util

import cats.data.NonEmptyList
import cats.syntax.option._
import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.standardCoreAbilities
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.FireBolt
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Monster, Werewolf, Zombie}
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

    def withDexteritySavingThrowScore(dexScore: Int) = {
      val savingThrowScores = testMonster.savingThrowScores.map {
        case (Dexterity, _) => Dexterity -> dexScore
        case (attribute, score) => attribute -> score
      }

      _savingThrowScores.set(savingThrowScores)(testMonster)
    }

    def withCombatIndex(index: Int) = Combatant(index, testMonster)
  }

  implicit class MonsterOps(val monster: Monster) extends AnyVal {
    import Monster._

    def withArmourClass(ac: Int) = monsterArmourClassLens.set(ac)(monster)
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

    def withResistance(creatureRes: DamageType*) =
      creatureResistancesLens.set(creatureRes.toList)(creature)
    def withImmunity(creatureImm: DamageType*) =
      creatureImmunitiesLens.set(creatureImm.toList)(creature)
    def withNoResistances()             = creatureResistancesLens.set(List.empty)(creature)
    def withNoImmunities()              = creatureImmunitiesLens.set(List.empty)(creature)
    def withNoResistancesOrImmunities() = creature.withNoResistances().withNoImmunities()

    def withCondition(condition: Condition) = creatureConditionsLens.set(List(condition))(creature)
    def withConditions(conditions: Condition*) =
      creatureConditionsLens.set(conditions.toList)(creature)

    def withAttackStatus(attackStatus: AttackStatus) =
      creatureAttackStatusLens.set(attackStatus)(creature)
    def withDefenseStatus(defenseStatus: AttackStatus) =
      creatureDefenseStatusLens.set(defenseStatus)(creature)

    def withLevel(level: Level)     = creatureLevelOptional.set(level)(creature)
    def withCombatIndex(index: Int) = Combatant(index, creature)

    def withSkills(perception: Int, stealth: Int) =
      creatureSkillsOptional.set(Skills(perception, stealth))(creature)
  }

  implicit class PlayerOps(val player: Player) extends AnyVal {
    import Player._

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
    def withReactionUsed()    = _reactionUsed.set(true)(fighter)
  }

  implicit class ChampionOps(val champion: Champion) extends AnyVal {
    import Champion._

    def withAbilitiesUsed(secondWindUsed: Boolean, actionSurgeUsed: Boolean) =
      _abilityUsages.set(BaseFighterAbilities(secondWindUsed, actionSurgeUsed))(champion)
  }

  implicit class ClericOps(val cleric: Cleric) extends AnyVal {
    import Cleric._

    def withConcentrating(concentratingSpell: Option[Spell]) =
      _concentratingSpell.set(concentratingSpell)(cleric)

    def withCantrip(cantrip: Spell) = _cantripKnown.set(cantrip.some)(cleric)
    def withNoCantrip()             = _cantripKnown.set(none[Spell])(cleric)
    def withSpellKnown(spell: Spell) =
      _spellsKnown.set(Map((spell.spellLevel, spell.spellEffect) -> spell))(cleric)
    def withSpellsKnown(spells: Spell*) =
      _spellsKnown.set(spells.map(spell => (spell.spellLevel, spell.spellEffect) -> spell).toMap)(
        cleric)
    def withAllSpellSlotsAvailableForLevel(level: Level) =
      _spellSlots.set(clericSpellSlots(level))(cleric)
    def withNoSpellSlotsAvailable() =
      _spellSlots.set(
        SpellSlots(FirstLevelSpellSlots(0), SecondLevelSpellSlots(0), ThirdLevelSpellSlots(0)))(
        cleric)
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

    def withCantrip(cantrip: Spell) = _cantripKnown.set(cantrip.some)(wizard)
    def withNoCantrip()             = _cantripKnown.set(none[Spell])(wizard)

    def withNoSpellSlotsAvailable() =
      _spellSlots.set(
        SpellSlots(FirstLevelSpellSlots(0), SecondLevelSpellSlots(0), ThirdLevelSpellSlots(0)))(
        wizard)

    def withCastShieldOnReaction(willCast: Boolean) = _castShieldAsReaction.set(willCast)(wizard)
    def withMageArmourPrepared(prepared: Boolean) = _mageArmourPrepared.set(prepared)(wizard)
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

  implicit val arbSpellSlots: Arbitrary[SpellSlots] = Arbitrary {
    for {
      firstLevelSpellSlots  <- arbFirstLevelSpellSlot.arbitrary
      secondLevelSpellSlots <- arbSecondLevelSpellSlot.arbitrary
      thirdLevelSpellSlots  <- arbThirdLevelSpellSlot.arbitrary
    } yield SpellSlots(firstLevelSpellSlots, secondLevelSpellSlots, thirdLevelSpellSlots)
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

        val resistances: List[DamageType]     = List.empty
        val immunities: List[DamageType]      = List.empty
        val bonusActionUsed: Boolean          = false
        val reactionUsed: Boolean             = false
        val name: String                      = n
        val abilities: List[CombatantAbility] = standardCoreAbilities
        val conditions: List[Condition]       = List.empty
        val attackStatus: AttackStatus        = Regular
        val defenseStatus: AttackStatus       = Regular

        val skills: Skills = creatureSkills

        val reactionOnHit: Option[OnHitReaction]       = None
        val reactionOnDamage: Option[OnDamageReaction] = None

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

        val armour: Armour                     = creature.armour
        val offHand: Option[Equipment]         = creature.offHand
        val armourClass: Int                   = creature.armourClass
        val proficiencyBonus: ProficiencyBonus = profBonus
        val resistances: List[DamageType]      = creature.resistances
        val immunities: List[DamageType]       = creature.immunities
        val name: String                       = creature.name
        val abilities: List[CombatantAbility]  = creature.abilities
        val conditions: List[Condition]        = List.empty
        val attackStatus: AttackStatus         = creature.attackStatus
        val defenseStatus: AttackStatus        = creature.defenseStatus

        val skills: Skills = creature.skills

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
        creature.resistances,
        creature.immunities,
        List.empty, // TODO add core abilities?
        creature.conditions,
        reactionUsed = false,
        creature.attackStatus,
        creature.defenseStatus,
        creatureType,
        cr,
        arbSkills.perception,
        arbSkills.stealth,
        TestMonster.defaultScores,
        creature.name
      )
  }

  implicit val arbFighterFightingStyle: Arbitrary[Seq[FighterFightingStyle]] = Arbitrary {
    Gen.someOf(Archery, Defense, Dueling, GreatWeaponFighting, Protection, TwoWeaponFighting)
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
        player.resistances,
        player.immunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Fighter.standardFighterAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
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
        player.resistances,
        player.immunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Champion.standardChampionAbilities,
        player.conditions,
        player.attackStatus,
        player.defenseStatus,
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
        player.resistances,
        player.immunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Barbarian.standardBarbarianAbilities,
        player.conditions,
        inRage = false,
        rageTurnsLeft = 10,
        attackStatus = player.attackStatus,
        defenseStatus = player.defenseStatus,
        name = player.name
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
        player.resistances,
        player.immunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Barbarian.standardBarbarianAbilities,
        player.conditions,
        inRage = false,
        rageTurnsLeft = 10,
        attackStatus = player.attackStatus,
        defenseStatus = player.defenseStatus,
        name = player.name
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
        SacredFlame.some,
        Cleric.clericSpellSlots(level),
        Cleric.standardClericSpellList,
        channelDivinityUsed = false,
        player.armour,
        player.offHand,
        Cleric.standardClericAbilities,
        player.conditions,
        player.proficiencyBonus,
        player.resistances,
        player.immunities,
        bonusActionUsed = player.bonusActionUsed,
        reactionUsed = player.reactionUsed,
        attackStatus = player.attackStatus,
        defenseStatus = player.defenseStatus,
        concentratingSpell = None,
        name = player.name
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
        player.resistances,
        player.immunities,
        player.bonusActionUsed,
        player.reactionUsed,
        Rogue.standardRogueAbilities,
        conditions = player.conditions,
        attackStatus = player.attackStatus,
        defenseStatus = player.defenseStatus,
        name = player.name
      )
  }

  implicit val arbWizard: Arbitrary[Wizard] = Arbitrary {
    for {
      player <- arbPlayer.arbitrary
      level  <- arbLevel.arbitrary
    } yield {
      Wizard(
        level,
        player.health,
        player.health,
        player.stats,
        player.baseWeapon,
        player.skills,
        FireBolt.some,
        Wizard.wizardSpellSlots(player.level),
        Wizard.standardWizardSpellList,
        castShieldAsReaction = true,
        mageArmourPrepared = true,
        NoArmour,
        none[Equipment],
        Wizard.standardWizardAbilities,
        player.conditions,
        player.proficiencyBonus,
        player.resistances,
        player.immunities,
        player.bonusActionUsed,
        player.reactionUsed,
        player.attackStatus,
        player.defenseStatus,
        concentratingSpell = none[Spell],
        name = player.name
      )
    }
  }
}
