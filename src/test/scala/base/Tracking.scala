package base
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, BonusAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Paralyzed}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Focus

trait Tracking {

  var swordUsedCount = 0
  val trackedSword = Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = false, {
    swordUsedCount += 1
    1
  })

  var offHAndSwordUsedCount = 0
  val trackedOffHandSword = Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = false, {
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
  def trackedMeleeSpellAttack(spellLvl: SpellLevel, concentration: Boolean = false): Spell =
    new SingleTargetAttackSpell() {
      val damageType: DamageType         = Fire
      val name: String                   = s"tracked-melee-spell-${spellLvl.value}"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneAction
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration: Boolean = concentration

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        meleeSpellUsedCount += 1
        4
      }
    }

  var trackedHealingSpellUsed = false
  var trackedHealingSpellUsedCount = 0
  def trackedHealingSpell(spellLvl: SpellLevel, healingDone: Int = 1): Spell = new SingleTargetHealingSpell {
    val name: String                   = "tracked-healing-spell"
    val school: SchoolOfMagic          = Evocation
    val castingTime: CastingTime       = OneAction
    val spellLevel: SpellLevel         = spellLvl
    val requiresConcentration: Boolean = false

    def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
      trackedHealingSpellUsed = true
      trackedHealingSpellUsedCount += 1
      healingDone
    }
  }

  var savingThrowSpellUsedCount = 0
  def trackedSavingThrowSpell(spellLvl: SpellLevel, savingAttribute: Attribute, damageOnSave: Boolean = false): Spell =
    new SingleTargetSavingThrowSpell() {
      val attribute: Attribute           = savingAttribute
      val halfDamageOnSave: Boolean      = damageOnSave

      val damageType: DamageType         = Fire
      val name: String                   = "tracked-saving-throw-spell-test"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneAction
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration: Boolean = false

      def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        savingThrowSpellUsedCount += 1
        4
      }
    }

  var conditionSpellUsedCount = 0
  def trackedConditionSpell(spellLvl: SpellLevel, singleTargetSpell: Boolean = true): ConcentrationConditionSpell =
    new ConcentrationConditionSpell() {
      val name: String = s"tracked-melee-spell-${spellLvl.value}"

      val attribute: Attribute  = Constitution
      val singleTarget: Boolean = singleTargetSpell

      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneAction
      val spellLevel: SpellLevel   = spellLvl

      def conditionFrom(spellCaster: SpellCaster): Condition =
        Paralyzed(10, 10, attribute, "tracked-condition-spell")

      override def effect[_: RS](spellCaster: SpellCaster,
                                 spellLevel: SpellLevel,
                                 targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
        conditionSpellUsedCount += 1

        (spellCaster, targets)
      }
    }

  var trackedConditionHandledCount = 0
  def trackedCondition(dc: Int, turnMissed: Boolean = false): Condition = new Condition {
    val name                    = "tracked-condition"
    val missesTurn              = turnMissed
    val handleOnDamage: Boolean = false

    val saveDc: Int    = dc
    val turnsLeft: Int = 10

    def handle[_: RS](creature: Creature): Creature = {
      trackedConditionHandledCount += 1
      creature
    }

    def handleOnDamage[_: RS](creature: Creature): Creature = creature
  }

  var trackedBonusActionUsed      = false
  def trackedBonusAction(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val name: String     = "test-tracked--bonus-ability"
    val order            = currentOrder
    val levelRequirement = LevelOne
    val abilityAction    = BonusAction

    def triggerMet(others: List[Combatant]) = true
    def conditionMet: Boolean               = true

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      (combatant, others)
    }

    def update: Creature = {
      trackedBonusActionUsed = true
      Player.playerBonusActionUsedLens.set(true)(combatant.creature.asInstanceOf[Player])
    }
  }
}
