package io.github.tjheslin1.dmspredictor.monsters

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import io.github.tjheslin1.dmspredictor.util.IntOps._

@Lenses("_") case class Vampire(health: Int,
                                maxHealth: Int,
                                stats: BaseStats = BaseStats(18, 18, 18, 17, 15, 18),
                                armourClass: Int = 12,
                                baseWeapon: Weapon = Vampire.UnarmedStrike,
                                armour: Armour = NoArmour,
                                offHand: Option[Equipment] = None,
                                resistances: List[DamageType] =
                                  List(Necrotic, Bludgeoning, Piercing, Slashing),
                                immunities: List[DamageType] = List.empty[DamageType],
                                conditions: List[Condition] = List.empty,
                                attackStatus: AttackStatus = Regular,
                                defenseStatus: AttackStatus = Regular,
                                radiantDamageTaken: Boolean = false,
                                name: String = NameGenerator.randomName)
    extends Monster {

  val challengeRating: Double = 3.0

  val creatureType: CreatureType = Undead

  val abilities: List[CombatantAbility] = List.empty[CombatantAbility]

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature = {
    if (radiantDamageTaken) copy(radiantDamageTaken = false)
    else {
      val regeneratedHp = Math.min(maxHealth, health + 20)
      Creature.creatureHealthLens.set(regeneratedHp)(copy(radiantDamageTaken = false))
      }
    }
}

object Vampire {

  case object UnarmedStrike extends Weapon {
    val name: String           = "Unarmed Strike (Vampire)"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Bludgeoning
    val twoHanded: Boolean     = true

    override val hitBonus: Int = 9

    def damage(implicit rollStrategy: RollStrategy): Int = (1 * D8) + 4
  }

  val strengthLens: Lens[Vampire, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Vampire, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Vampire, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Vampire, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Vampire, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Vampire, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
