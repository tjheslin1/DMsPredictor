package io.github.tjheslin1.util

object NameGenerator {

  val vowels = List('a', 'e', 'i', 'o', 'u')

  val prefixStart = List('b', 'c', 'd', 'f')
  val centreStart = List('s', 't', 'v', 'w')
  val suffixStart = List('k', 'l', 'm', 'o')

  def shuffled(cs: Seq[Char]) = scala.util.Random.shuffle(cs).head.toString

  def randomName =
    shuffled(prefixStart) + shuffled(vowels) + "'" + shuffled(centreStart) + shuffled(vowels) + "'" + shuffled(
      suffixStart) + shuffled(vowels)
}
