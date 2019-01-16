# DMsPredictor [![Build Status](https://travis-ci.com/tjheslin1/DMsPredictor.svg?branch=master)](https://travis-ci.com/tjheslin1/DMsPredictor)
Combat encounter results predictor for DMs for Dungeons &amp; Dragons 5th Edition

Assumptions:

##### Resources will be exhausted:
The simulation is designed around a single encounter, therefore abilities such as spellcasting are prioritised per 
class and will used starting from the highest spell slot to the lowest as well as using other abilities 
(e.g. a Sorceror's twinned spell).
 
##### Player knowledge:

Players typically have an idea of the enemies they are facing or learn during the encounter if a particular strategy 
isn't working (e.g. a monster is immune to a particular type of damage). Therefore the assumption is made that the player
will choose the most advantageous choice they have.

For example: An `Eldritch Knight` will start casting cantrips if the enemy is resistant or immune to their weapon.