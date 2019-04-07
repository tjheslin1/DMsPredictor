package io.github.tjheslin1.dmspredictor.classes.rogue

import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogue.calculateArmourClass
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogueAbilities._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Rogue(level: Level,
                              health: Int,
                              maxHealth: Int,
                              stats: BaseStats,
                              baseWeapon: Weapon,
                              armour: Armour = NoArmour,
                              offHand: Option[Equipment] = None,
                              proficiencyBonus: ProficiencyBonus = 0,
                              resistances: List[DamageType] = List.empty,
                              immunities: List[DamageType] = List.empty,
                              bonusActionUsed: Boolean = false,
                              abilities: List[CombatantAbility] = ???,
                              stealthProficiency: Boolean = true,
                              hiddenFrom: List[Combatant] = List.empty,
                              conditions: List[Condition] = List.empty,
                              attackStatus: AttackStatus = Regular,
                              defenseStatus: AttackStatus = Regular,
                              name: String = NameGenerator.randomName)
    extends BaseRogue {

  val savingThrowProficiencies: NonEmptyList[Attribute] = NonEmptyList.of(Dexterity, Intelligence)

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  // TODO: Uncanny dodge
  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))
}

object Rogue {

  val standardRogueAbilities: List[CombatantAbility] = List(
    hide(1),
    sneakAttack(2),
    twoWeaponFighting(3)
  )

  val strengthLens: Lens[Rogue, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Rogue, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Rogue, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Rogue, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Rogue, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Rogue, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
