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

  var trackedOnWeaponDamageUsedCount = 0
  var trackedOnWeaponDamageUsed      = false
  def trackedOnWeaponDamageAbility(currentOrder: Int,
                                   level: Level = LevelOne,
                                   trigger: => Boolean = true,
                                   updatesTracking: => Unit = trackedOnWeaponDamageUsed = true,
                                   dmg: => Int = 1)(combatant: Combatant): Ability =
    new OnWeaponDamageAbility(combatant) {
      val name                    = "tracked-on-weapon-damage-ability"
      val order                   = currentOrder
      val levelRequirement: Level = level

      def triggerMet(others: List[Combatant]): Boolean = trigger

      def conditionMet: Boolean = true

      def damage[_: RS](): Int = {
        trackedOnWeaponDamageUsedCount += 1

        dmg
      }

      def update: Creature = {
        updatesTracking

        combatant.creature
      }
    }

  var meleeSpellUsedCount = 0
  def trackedMeleeSpellAttack(spellLvl: SpellLevel,
                              concentration: Boolean = false,
                              higherSpellSlot: Boolean = true,
                              dmg: Int = 4,
                              halfDamageOnAMiss: Boolean = false,
                              additionalTargetEffect: Combatant => Combatant = c => c): Spell =
    new SingleTargetAttackSpell() {
      val damageType: DamageType      = Fire
      val name: String                = s"tracked-melee-spell-${spellLvl.value}"
      val school: SchoolOfMagic       = Evocation
      val castingTime: CastingTime    = OneActionCast
      val spellLevel: SpellLevel      = spellLvl
      val requiresConcentration       = concentration
      val benefitsFromHigherSpellSlot = higherSpellSlot
      val halfDamageOnMiss            = halfDamageOnAMiss

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        meleeSpellUsedCount += 1

        dmg
      }

      override def additionalEffect(target: Combatant, attackResult: AttackResult): Combatant =
        additionalTargetEffect(target)
    }

  var multiMeleeSpellUsedCount = 0
  def trackedMultiMeleeSpellAttack(spellLvl: SpellLevel,
                                   savingThrowAttribute: Attribute = Dexterity,
                                   concentration: Boolean = false,
                                   higherSpellSlot: Boolean = true): Spell =
    new MultiTargetSavingThrowSpell {
      val name: String           = s"tracked-multi-melee-spell-${spellLvl.value}"
      val damageType: DamageType = Fire

      val school: SchoolOfMagic       = Evocation
      val castingTime: CastingTime    = OneActionCast
      val spellLevel: SpellLevel      = spellLvl
      val requiresConcentration       = concentration
      val attribute: Attribute        = savingThrowAttribute
      val halfDamageOnSave            = false
      val benefitsFromHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        multiMeleeSpellUsedCount += 1
        4
      }
    }

  var trackedHealingSpellUsed      = false
  var trackedHealingSpellUsedCount = 0
  def trackedHealingSpell(spellLvl: SpellLevel,
                          healingDone: Int = 1,
                          higherSpellSlot: Boolean = true): Spell =
    new SingleTargetHealingSpell {
      val name: String                   = "tracked-healing-spell"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration: Boolean = false
      val benefitsFromHigherSpellSlot    = higherSpellSlot

      def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        trackedHealingSpellUsed = true
        trackedHealingSpellUsedCount += 1
        healingDone
      }
    }

  var singleSavingThrowSpellUsedCount = 0
  def trackedSingleTargetSavingThrowSpell(
      spellLvl: SpellLevel,
      savingAttribute: Attribute,
      damageOnSave: Boolean = false,
      higherSpellSlot: Boolean = true,
      additionalTargetEffect: (SpellCaster,
                               Combatant,
                               List[Combatant],
                               Boolean) => (SpellCaster, Combatant, List[Combatant]) =
        (c, t, o, s) => (c, t, o)): Spell =
    new SingleTargetSavingThrowSpell() {
      val savingThrowAttribute: Attribute = savingAttribute
      val halfDamageOnSave: Boolean       = damageOnSave

      val damageType: DamageType      = Fire
      val name: String                = "tracked-single-target-saving-throw-spell-test"
      val school: SchoolOfMagic       = Evocation
      val castingTime: CastingTime    = OneActionCast
      val spellLevel: SpellLevel      = spellLvl
      val requiresConcentration       = false
      val benefitsFromHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        singleSavingThrowSpellUsedCount += 1
        4
      }

      override def additionalEffect[_: RS](
          spellCaster: SpellCaster,
          target: Combatant,
          others: List[Combatant],
          savingThrowPassed: Boolean): (SpellCaster, Combatant, List[Combatant]) =
        additionalTargetEffect(spellCaster, target, others, savingThrowPassed)
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

      val name: String                = "tracked-multi-target-saving-throw-spell-test"
      val school: SchoolOfMagic       = Evocation
      val castingTime: CastingTime    = OneActionCast
      val spellLevel: SpellLevel      = spellLvl
      val requiresConcentration       = false
      val benefitsFromHigherSpellSlot = higherSpellSlot

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        multiSavingThrowSpellUsedCount += 1
        4
      }
    }

  var conditionSpellUsedCount = 0
  def trackedConditionSpell(spellLvl: SpellLevel,
                            savingThrowAttribute: Attribute = Constitution,
                            singleTargetSpell: Boolean = true,
                            higherSpellSlot: Boolean = true): ConcentrationConditionSpell =
    new ConcentrationConditionSpell {
      val name: String = s"tracked-melee-spell-${spellLvl.value}"

      val attribute: Attribute  = savingThrowAttribute
      val singleTarget: Boolean = singleTargetSpell

      val school: SchoolOfMagic       = Evocation
      val castingTime: CastingTime    = OneActionCast
      val spellLevel: SpellLevel      = spellLvl
      val benefitsFromHigherSpellSlot = higherSpellSlot

      def conditionFrom(spellCaster: SpellCaster): Condition =
        Paralyzed(10, 10, attribute)

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
                           castingAction: CastingTime = OneActionCast,
                           concentration: Boolean = false,
                           higherSpellSlot: Boolean = true): SelfBuffSpell = new SelfBuffSpell {

    val name: String                 = s"tracked-self-buff-spell-${spellLvl.value}"
    val selfBuffCondition: Condition = buffCondition

    val school: SchoolOfMagic       = Divination
    val castingTime: CastingTime    = castingAction
    val spellLevel: SpellLevel      = spellLvl
    val requiresConcentration       = concentration
    val benefitsFromHigherSpellSlot = higherSpellSlot

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

  def setHealthToOneEffect(spellCaster: SpellCaster,
                           target: Combatant): (SpellCaster, Combatant) = {
    val oneHpTarget = (Combatant.creatureLens composeLens Creature.creatureHealthLens) .set(1)(target)

    (spellCaster, oneHpTarget)
  }

  var trackedInstantEffectUsed = false
  var trackedInstantEffectSpellLevelUsed = -1
  def trackedInstantEffectSpell(spellLvl: SpellLevel,
                                 trackedEffect: (SpellCaster, Combatant) => (SpellCaster, Combatant) =
        setHealthToOneEffect,
                                 higherSpellSlot: Boolean = false): SingleTargetInstantEffectSpell =
    new SingleTargetInstantEffectSpell {
      val name                        = s"tracked-instant-effect-spell-${spellLvl.value}"
      val school                      = Enchantment
      val castingTime                 = OneActionCast
      val spellLevel                  = spellLvl
      val requiresConcentration       = false
      val benefitsFromHigherSpellSlot = higherSpellSlot

      def instantEffect(spellCaster: SpellCaster, spellLevel: SpellLevel, target: Combatant): (SpellCaster, Combatant) = {
        trackedInstantEffectUsed = true
        trackedInstantEffectSpellLevelUsed = spellLevel.value

        trackedEffect(spellCaster, target)
      }
    }

  var trackedStartOfTurnConditionHandledCount = 0
  var trackedStartOfTurnConditionDecremented  = false
  def trackedStartOfTurnCondition(dc: Int,
                                  turnMissed: Boolean = false,
                                  turns: Int = 10): Condition =
    new StartOfTurnCondition {
      val name                       = "tracked-start-of-turn-condition"
      val missesTurn                 = turnMissed
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

      def onConditionApplied(creature: Creature): Creature = creature
      def onConditionRemoved(creature: Creature): Creature = creature
    }

  var trackedEndOfTurnConditionHandledCount = 0
  def trackedEndOfTurnCondition(dc: Int,
                                turnMissed: Boolean = false,
                                handledOnDamage: Boolean = false): Condition =
    new EndOfTurnCondition {
      val name              = "tracked-end-of-turn-condition"
      val missesTurn        = turnMissed
      val isHandledOnDamage = handledOnDamage

      val saveDc: Int    = dc
      val turnsLeft: Int = 10

      def decrementTurnsLeft(): Condition = this

      def handleEndOfTurn[_: RS](creature: Creature): Creature = {
        trackedEndOfTurnConditionHandledCount += 1
        creature
      }

      def onConditionApplied(creature: Creature): Creature = creature
      def onConditionRemoved(creature: Creature): Creature = creature
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

  def trackedCondition(saveDC: Int,
                       creatureMissesTurn: Boolean,
                       handledOnDamage: Boolean = false,
                       conditionTurnsLeft: Int = 10,
                       startOfTurn: Creature => Creature = c => c,
                       endOfTurn: Creature => Creature = c => c,
                       onDamage: Creature => Creature = c => c,
                       onApplied: Creature => Creature = c => c,
                       onRemoved: Creature => Creature = c => c): Condition = new Condition {
    val name = "tracked-condition"
    val saveDc = saveDC
    val turnsLeft = conditionTurnsLeft
    val missesTurn = creatureMissesTurn
    val isHandledOnDamage = handledOnDamage

    def decrementTurnsLeft(): Condition = this

    def handleStartOfTurn[_: RS](creature: Creature): Creature = startOfTurn(creature)

    def handleEndOfTurn[_: RS](creature: Creature): Creature = endOfTurn(creature)

    def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = onDamage(creature)

    def onConditionApplied(creature: Creature): Creature = onApplied(creature)

    def onConditionRemoved(creature: Creature): Creature = onRemoved(creature)
  }
}
