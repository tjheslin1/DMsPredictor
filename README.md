# DMsPredictor [![Build Status](https://travis-ci.com/tjheslin1/DMsPredictor.svg?branch=master)](https://travis-ci.com/tjheslin1/DMsPredictor) [![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-brightgreen.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
Combat results predictor for DMs for Dungeons & Dragons 5th Edition.

https://www.dmspredictor.com

## Classes:

- [Barbarian](src/main/scala/io/github/tjheslin1/dmspredictor/classes/barbarian/README.md)
- [Cleric](src/main/scala/io/github/tjheslin1/dmspredictor/classes/cleric/README.md)
- [Fighter](src/main/scala/io/github/tjheslin1/dmspredictor/classes/fighter/README.md)
- [Rogue](src/main/scala/io/github/tjheslin1/dmspredictor/classes/rogue/README.md)
- [Wizard](src/main/scala/io/github/tjheslin1/dmspredictor/classes/wizard/README.md)

## Monsters

This list only includes monsters with README's

- [Vampire](src/main/scala/io/github/tjheslin1/dmspredictor/monsters/vampire/README.md)

## Requests:

This simulator implements what is available in the
[Systems Reference Document (SRD)](http://dnd.wizards.com/articles/features/systems-reference-document-srd)
provide by Wizard of the Coast.

If a spell or anything else is not present in the simulator that you would like to use, please raise an _Issue_ requesting this,
or contribute yourself via a _Pull Request_!

Assumptions:

##### Resources will be exhausted:
The simulation is designed around a single encounter, therefore abilities such as spellcasting are prioritised per 
class and will be used starting from the highest spell slot to the lowest as well as using other abilities 
(e.g. a Sorceror's twinned spell).

##### Proficiency with used weapons and armour:
The simulation assumes that players have proficiency with the equipment it uses.

##### Ability Score Increases

Ability score increases received at levels 4, 8, etc are not modelled and are assumed to be included in
 the characters stats upon submission to the simulator.

##### No validation against a Creature's Stats are made.

For example:

A Fighter could have `BaseStats(1, 1, 1, 1, 1, 1)` or `BaseStats(24, 24, 24, 24, 24, 24)`.

## Running the project locally:

The simulation is configured via JSON:

NOTE: that the maximum number of simulations is currently set to 10,000.


`sbt 'run "{\"simulationName\":\"Rogue vs Goblins\"........`

```json
{
 "simulationName": "Wizard vs Goblin",
 "simulations": 100,
 "focus": "LowestFirst",
 "players": [
   {
    "class": "wizard",
    "level": 4,
    "stats": "10,10,14,14,14,10",
    "weapon": "Shortsword",
    "skills": "1,1",
    "name": "TestWizard"
   }
 ],
 "monsters": [
  {
    "monster": "goblin",
    "name": "TestGoblin"
  }
 ]
}
```

Otherwise you can create your players and monsters in the Main class to save having to fiddle with JSON.

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
DEBUG i.g.t.d.m.vampire.VampireAbilities$ - Bite (Vampire) deals 0 necrotic damage
DEBUG i.g.t.d.classes.CoreAbilities$ - Barbarian used Extra Attack
DEBUG i.g.t.d.c.b.BaseBarbarianAbilities$ - Barbarian is recklessly attacking
DEBUG i.g.t.dmspredictor.model.Actions$ - D20.roll() of 19
DEBUG i.g.t.d.model.AdjustedDamage$ - Vampire took 5 (adjusted) Slashing damage
DEBUG i.g.t.d.c.b.BaseBarbarianAbilities$ - Barbarian is recklessly attacking
DEBUG i.g.t.dmspredictor.model.Actions$ - D20.roll() of 17
DEBUG i.g.t.d.model.AdjustedDamage$ - Vampire took 5 (adjusted) Slashing damage
DEBUG i.g.t.d.model.condition.Grappled - Cleric is still Grappled
DEBUG i.g.t.d.c.c.BaseClericAbilities$ - Cleric used Destroy Undead
DEBUG i.g.t.d.simulation.BasicSimulation - pc: Barbarian - hp=40
DEBUG i.g.t.d.simulation.BasicSimulation - pc: Cleric - hp=28
DEBUG i.g.t.d.simulation.BasicSimulation - mob: Vampire - hp=134
```
