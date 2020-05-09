# Spellbook

This readme is to document design decision made when implementing spells

## Ranger Spells

### Hunter's Mark

Instead of applying the Hunter's Mark condition on enemies and then handling the movement of the mark 
onto others when they drop to 0 hit points, I decided that it would be easier and just as 
effective to apply the condition to the Ranger itself, following the assumption that the Ranger 
will focus it's weapon attacks on the creature it would have marked. This is based on experience 
of playing 5th edition D&D.

As the bonus damage only applies to weapon attacks multi-target spells are not of a concern here.

Once 11th level features of the Ranger are implemented (see Multiattack on page 38 of System Reference Document 5.1) 
this will have to be addressed as they allow for multi-target weapon attacks.   