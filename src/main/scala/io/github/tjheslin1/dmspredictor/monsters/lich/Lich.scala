package io.github.tjheslin1.dmspredictor.monsters.lich

import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.Armour
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned}
import io.github.tjheslin1.dmspredictor.model.reaction.OnHitReaction
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Lich(
    health: Int,
    maxHealth: Int,
    stats: BaseStats = BaseStats(11, 16, 16, 20, 14, 16),
    baseWeapon: Weapon = ParalyzingTouch,
    armour: Armour = LichNaturalArmour,
    offHand: Option[Equipment] = None,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List(Cold, Lightning, Necrotic),
    damageImmunities: List[DamageType] = List(Poison, Bludgeoning, Piercing, Slashing),
    conditionResistances: List[Condition] = List.empty[Condition], // TODO add Turned,
    conditionImmunities: List[Condition] = List.empty[Condition],
    conditions: List[Condition] = List.empty[Condition],
    reactionUsed: Boolean = false,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    spellSlots: SpellSlots = SpellSlots(4, 3, 3, 3, 3, 1, 1, 1, 1),
    spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = standardLichSpellList,
    concentratingSpell: Option[Spell] = none[Spell],
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends Monster
    with SpellCaster {

  val spellCastingLevel: Level = LevelEighteen
  val spellCastingModifier     = 7
  val armourClass: Int         = calculateArmourClass(stats, conditions)

  val challengeRating = 21.0
  val skills          = Skills(perception = 9, stealth = stats.dexterity.value)

  val savingThrowScores: Map[Attribute, Int] = Map(
    Strength     -> mod(stats.strength),
    Dexterity    -> mod(stats.dexterity),
    Constitution -> 10,
    Wisdom       -> 9,
    Intelligence -> 12,
    Charisma     -> mod(stats.charisma)
  )

  val creatureType = Undead

  val abilities: List[CombatantAbility] = lichAbilities

  def weapon[_: RS]: Weapon = baseWeapon

  // TODO Legendary resistances
  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Lich =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20

  // TODO Legendary resistances
  def resetStartOfTurn(): Creature = this

  val levelSpellcastingLearned = LevelOne

  override val reactionOnHit: Option[OnHitReaction] = ShieldSpell.some
}

object Lich {
  def calculateHealth[_: RS](): Int = (18 * D8) + 54

  def calculateArmourClass(stats: BaseStats, conditions: List[Condition]): Int = {
    val baseArmourClass = LichNaturalArmour.armourClass(stats.dexterity)

    if (conditions.contains(ShieldBuffCondition())) 5 + baseArmourClass
    else baseArmourClass
  }

  val lichAbilities: List[CombatantAbility] = List(
    castMultiTargetOffensiveSpell(1),
    castSingleTargetOffensiveSpell(2)
  )

  case object LichNaturalArmour extends Armour {
    val name = "Lich Natural Armour"

    def armourClass(dexterity: Stat): Int = 17
  }

  // TODO Paralyzing effect
  case object ParalyzingTouch extends Weapon {
    val name                   = "Lich Natural Armour"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Cold
    val twoHanded              = false
    val finesse: Boolean       = false

    def damage(implicit rollStrategy: RollStrategy): Int = 3 * D6
  }

  val standardLichSpellList: Map[(SpellLevel, SpellEffect), Spell] =
    Map(
      (FireBolt.spellLevel, FireBolt.spellEffect)         -> FireBolt,
      (MagicMissile.spellLevel, MagicMissile.spellEffect) -> MagicMissile,
      (AcidArrow.spellLevel, AcidArrow.spellEffect)       -> AcidArrow,
      (Fireball.spellLevel, Fireball.spellEffect)         -> Fireball,
      (Blight.spellLevel, Blight.spellEffect)             -> Blight,
      (Disintegrate.spellLevel, Disintegrate.spellEffect) -> Disintegrate
      // TODO
    )

  val strengthLens: Lens[Lich, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Lich, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Lich, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Lich, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Lich, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Lich, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
