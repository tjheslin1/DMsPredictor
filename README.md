# DMsPredictor [![Build Status](https://travis-ci.com/tjheslin1/DMsPredictor.svg?branch=master)](https://travis-ci.com/tjheslin1/DMsPredictor)
Combat encounter results predictor for DMs for Dungeons &amp; Dragons 5th Edition

## Requests:

If a subclass, spell or anything else is not present in the simulator, please raise an _Issue_ requesting this,
or contribute yourself via a _Pull Request_!

Assumptions:

##### Resources will be exhausted:
The simulation is designed around a single encounter, therefore abilities such as spellcasting are prioritised per 
class and will be used starting from the highest spell slot to the lowest as well as using other abilities 
(e.g. a Sorceror's twinned spell).
 
##### Player knowledge:

Players typically have an idea of the enemies they are facing or learn during the encounter if a particular strategy 
isn't working (e.g. a monster is immune to a particular type of damage). Therefore the assumption is made that the player
will choose the most advantageous choice they have.

For example: An _Eldritch Knight_ will cast spells/cantrips if the enemy is resistant or immune to their weapon.

##### Ability Score Increases

Ability score increases received at levels 4, 8, etc are not modelled and are assumed to be included in the characters stats.
No validation against a Creature's Stats are made.

For example:

A Fighter could have `BaseStats(1, 1, 1, 1, 1, 1)` or `BaseStats(24, 24, 24, 24, 24, 24)`.

##### Example debug logs:

```

DEBUG i.g.t.d.monsters.vampire.Vampire - Vampire regenerated health
DEBUG i.g.t.d.monsters.MonsterAbilities$ - Vampire used Multi attack (Monster): 2 attacks
DEBUG i.g.t.d.m.vampire.VampireAbilities$ - Vampire used Unarmed Strike (Vampire)
DEBUG i.g.t.dmspredictor.model.Actions$ - D20.roll() of 16
DEBUG i.g.t.d.m.vampire.VampireAbilities$ - Vampire grapples Cleric
DEBUG i.g.t.d.m.vampire.VampireAbilities$ - Vampire used Bite (Vampire)
DEBUG i.g.t.dmspredictor.model.Actions$ - D20.roll() of 3
DEBUG i.g.t.d.model.AdjustedDamage$ - Cleric took 0 (adjusted) Piercing damage
DEBUG i.g.t.d.model.condition.Charmed - Cleric is no longer Charmed
DEBUG i.g.t.d.m.vampire.VampireAbilities$ - Bite (Vampire) deals 0 necrotic damage
DEBUG i.g.t.d.classes.CoreAbilities$ - Totem Warrior used Extra Attack
DEBUG i.g.t.d.c.b.BaseBarbarianAbilities$ - Totem Warrior is recklessly attacking
DEBUG i.g.t.dmspredictor.model.Actions$ - D20.roll() of 19
DEBUG i.g.t.d.model.AdjustedDamage$ - Vampire took 5 (adjusted) Slashing damage
DEBUG i.g.t.d.c.b.BaseBarbarianAbilities$ - Totem Warrior is recklessly attacking
DEBUG i.g.t.dmspredictor.model.Actions$ - D20.roll() of 17
DEBUG i.g.t.d.model.AdjustedDamage$ - Vampire took 5 (adjusted) Slashing damage
DEBUG i.g.t.d.model.condition.Grappled - Cleric is still Grappled
DEBUG i.g.t.d.c.c.BaseClericAbilities$ - Cleric used Destroy Undead
DEBUG i.g.t.d.simulation.BasicSimulation - pc: Totem Warrior - hp=40
DEBUG i.g.t.d.simulation.BasicSimulation - pc: Cleric - hp=28
DEBUG i.g.t.d.simulation.BasicSimulation - mob: Vampire - hp=134
```