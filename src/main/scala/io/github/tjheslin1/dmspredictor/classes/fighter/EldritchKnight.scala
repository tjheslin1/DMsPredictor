package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.EldritchKnight._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class EldritchKnight(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
    abilityUsages: BaseFighterAbilities = BaseFighterAbilities.allUnused(),
    proficiencyBonus: ProficiencyBonus = 0,
    spellsKnown: Map[SpellLevel, Spell] = Map(ChromaticOrb.spellLevel -> ChromaticOrb),
    spellSlots: EldritchKnightSpellSlots = EldritchKnightSpellSlots(FirstLevelSpellSlot(2)),
    resistances: List[DamageType] = List.empty,
    immunities: List[DamageType] = List.empty,
    abilities: List[CreatureAbility] = standardEldritchKnightAbilities,
    name: String = NameGenerator.randomName)
    extends Creature
    with BaseFighter {

  import Fighter._

  val creatureType: CreatureType = PlayerCharacter

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth(modification: Int): EldritchKnight = copy(health = Math.max(0, health + modification))

  def scoresCritical(roll: Int): Boolean = if (level.value <= 2) roll == 20 else roll >= 19
}

object EldritchKnight {

  import BaseFighterAbilities._
  import EldritchKnightAbilities._

  val HitDice = D10

  val standardEldritchKnightAbilities: List[CreatureAbility] = List(
    1 -> actionSurge,
    2 -> secondWind,
    3 -> castSpell,
    4 -> extraAttack,
    5 -> twoWeaponFighting
  )

  implicit def eldritchKnightShow[_: RS]: Show[EldritchKnight] = Show.show { eldritchKnight =>
    s"EldritchKnight: " +
      s"Name: ${eldritchKnight.name}, " +
      s"health: ${eldritchKnight.health}, " +
      s"AC: ${eldritchKnight.armourClass}"
  }

  val strengthLens: Lens[EldritchKnight, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[EldritchKnight, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[EldritchKnight, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[EldritchKnight, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[EldritchKnight, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[EldritchKnight, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
