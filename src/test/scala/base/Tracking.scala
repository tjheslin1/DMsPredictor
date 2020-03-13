package base

import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Focus

trait Tracking {

  var swordUsedCount = 0
  val trackedSword = Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = false, {
    swordUsedCount += 1
    1
  })

  var offHAndSwordUsedCount = 0
  val trackedOffHandSword =
    Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = false, {
      offHAndSwordUsedCount += 1
      1
    })

  var trackedAbilityUsedCount = 0
  var trackedAbilityUsed      = false
  def trackedAbility(
      currentOrder: Int,
      level: Level = LevelOne,
      action: AbilityAction = WholeAction,
      condition: => Boolean = trackedAbilityUsed == false,
      useAbilityTracking: => Unit = trackedAbilityUsedCount += 1,
      updatedTracking: => Unit = trackedAbilityUsed = true)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val name: String     = s"tracked-ability-$currentOrder"
      val order            = currentOrder
      val levelRequirement = level
      val abilityAction    = action

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = condition

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        useAbilityTracking

        (combatant, others)
      }

      def update: Creature = {
        updatedTracking

        combatant.creature
      }
    }

  var otherTrackedAbilityUsedCount = 0
  var otherTrackedAbilityUsed      = false
  def otherTrackedAbility(
      order: Int,
      action: AbilityAction = WholeAction,
      `updatedTracking`: => Unit = otherTrackedAbilityUsed = true): CombatantAbility =
    trackedAbility(
      order,
      action = `action`,
      condition = otherTrackedAbilityUsed == false,
      useAbilityTracking = otherTrackedAbilityUsedCount += 1,
      updatedTracking = `updatedTracking`
    )

  var meleeSpellUsedCount = 0
  def trackedMeleeSpellAttack(spellLvl: SpellLevel, concentration: Boolean = false, higherSpellSlot: Boolean = true): Spell =
    new SingleTargetAttackSpell() {
      val damageType: DamageType         = Fire
      val name: String                   = s"tracked-melee-spell-${spellLvl.value}"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration: Boolean = concentration
      val useHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        meleeSpellUsedCount += 1
        4
      }
    }

  var multiMeleeSpellUsedCount = 0
  def trackedMultiMeleeSpellAttack(spellLvl: SpellLevel, concentration: Boolean = false, higherSpellSlot: Boolean = true): Spell =
    new MultiTargetSavingThrowSpell {
      val name: String           = s"tracked-multi-melee-spell-${spellLvl.value}"
      val damageType: DamageType = Fire

      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration = concentration
      val attribute: Attribute           = Dexterity
      val halfDamageOnSave      = false
      val useHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        multiMeleeSpellUsedCount += 1
        4
      }
    }

  var trackedHealingSpellUsed      = false
  var trackedHealingSpellUsedCount = 0
  def trackedHealingSpell(spellLvl: SpellLevel, healingDone: Int = 1, higherSpellSlot: Boolean = true): Spell =
    new SingleTargetHealingSpell {
      val name: String                   = "tracked-healing-spell"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration: Boolean = false
      val useHigherSpellSlot = higherSpellSlot

      def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        trackedHealingSpellUsed = true
        trackedHealingSpellUsedCount += 1
        healingDone
      }
    }

  var singleSavingThrowSpellUsedCount = 0
  def trackedSingleTargetSavingThrowSpell(spellLvl: SpellLevel,
                                          savingAttribute: Attribute,
                                          damageOnSave: Boolean = false,
                                          higherSpellSlot: Boolean = true): Spell =
    new SingleTargetSavingThrowSpell() {
      val attribute: Attribute      = savingAttribute
      val halfDamageOnSave: Boolean = damageOnSave

      val damageType: DamageType         = Fire
      val name: String                   = "tracked-single-target-saving-throw-spell-test"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration = false
      val useHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        singleSavingThrowSpellUsedCount += 1
        4
      }
    }

  var multiSavingThrowSpellUsedCount = 0
  def trackedMultiTargetSavingThrowSpell(spellLvl: SpellLevel,
                                         savingAttribute: Attribute,
                                         damageOnSave: Boolean = false,
                                         higherSpellSlot: Boolean = true): Spell =
    new MultiTargetSavingThrowSpell {
      val attribute: Attribute      = savingAttribute
      val halfDamageOnSave: Boolean = damageOnSave
      val damageType: DamageType    = Fire

      val name: String                   = "tracked-multi-target-saving-throw-spell-test"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration = false
      val useHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        multiSavingThrowSpellUsedCount += 1
        4
      }
    }

  var conditionSpellUsedCount = 0
  def trackedConditionSpell(spellLvl: SpellLevel,
                            singleTargetSpell: Boolean = true,
                            higherSpellSlot: Boolean = true): ConcentrationConditionSpell =
    new ConcentrationConditionSpell {
      val name: String = s"tracked-melee-spell-${spellLvl.value}"

      val attribute: Attribute  = Constitution
      val singleTarget: Boolean = singleTargetSpell

      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneActionCast
      val spellLevel: SpellLevel   = spellLvl
      val useHigherSpellSlot = higherSpellSlot

      def conditionFrom(spellCaster: SpellCaster): Condition =
        Paralyzed(10, 10, attribute, "tracked-condition-spell")

      override def effect[_: RS](spellCaster: SpellCaster,
                                 spellLevel: SpellLevel,
                                 targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
        conditionSpellUsedCount += 1

        (spellCaster, targets)
      }
    }

  var selfBuffSpellUsedCount            = 0
  var selfBuffSpellConcentrationHandled = false
  def trackedSelfBuffSpell(buffCondition: Condition,
                           spellLvl: SpellLevel,
                           castionAction: CastingTime = OneActionCast,
                           concentration: Boolean = false,
                           higherSpellSlot: Boolean = true): SelfBuffSpell = new SelfBuffSpell {

    val name: String                 = s"tracked-self-buff-spell-${spellLvl.value}"
    val selfBuffCondition: Condition = buffCondition

    val school: SchoolOfMagic          = Divination
    val castingTime: CastingTime       = castionAction
    val spellLevel: SpellLevel         = spellLvl
    val requiresConcentration = concentration
    val useHigherSpellSlot = higherSpellSlot

    override def effect[_: RS](spellCaster: SpellCaster,
                               spellLevel: SpellLevel,
                               targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
      selfBuffSpellUsedCount += 1

      super.effect(spellCaster, spellLevel, targets)
    }

    def onLossOfConcentration(spellCaster: SpellCaster, damage: Int): SpellCaster = {
      selfBuffSpellConcentrationHandled = true

      val updatedConditions = spellCaster.conditions diff List(selfBuffCondition)

      Creature.creatureConditionsLens.set(updatedConditions)(spellCaster).asInstanceOf[SpellCaster]
    }
  }

  var trackedStartOfTurnConditionHandledCount = 0
  var trackedStartOfTurnConditionDecremented  = false
  def trackedStartOfTurnCondition(dc: Int,
                                  turnMissed: Boolean = false,
                                  turns: Int = 10): Condition =
    new StartOfTurnCondition {
      val name                    = "tracked-start-of-turn-condition"
      val missesTurn              = turnMissed
      val isHandledOnDamage: Boolean = false

      val saveDc: Int    = dc
      val turnsLeft: Int = turns

      def decrementTurnsLeft(): Condition = {
        trackedStartOfTurnConditionDecremented = true
        trackedStartOfTurnCondition(dc, turnMissed, turnsLeft - 1)
      }

      def handleStartOfTurn[_: RS](creature: Creature): Creature = {
        trackedStartOfTurnConditionHandledCount += 1
        creature
      }
    }

  var trackedEndOfTurnConditionHandledCount = 0
  def trackedEndOfTurnCondition(dc: Int, turnMissed: Boolean = false): Condition =
    new EndOfTurnCondition {
      val name                    = "tracked-end-of-turn-condition"
      val missesTurn              = turnMissed

      val saveDc: Int    = dc
      val turnsLeft: Int = 10

      def decrementTurnsLeft(): Condition = this

      def handleEndOfTurn[_: RS](creature: Creature): Creature = {
        trackedEndOfTurnConditionHandledCount += 1
        creature
      }
    }

  var trackedBonusActionUsed = false
  def trackedBonusAction(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val name: String     = "test-tracked--bonus-ability"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = BonusAction

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = true

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) =
        (combatant, others)

      def update: Creature = {
        trackedBonusActionUsed = true
        Player.playerBonusActionUsedLens.set(true)(combatant.creature.asInstanceOf[Player])
      }
    }
}
