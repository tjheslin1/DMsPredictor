package io.github.tjheslin1.dmspredictor.model.ability

import cats.data.NonEmptyList

sealed trait AbilityAction extends Product with Serializable

case object WholeAction  extends AbilityAction
case object MultiAttack  extends AbilityAction
case object SingleAttack extends AbilityAction

object AbilityAction {
  val any = NonEmptyList.of(WholeAction, MultiAttack, SingleAttack)
}
