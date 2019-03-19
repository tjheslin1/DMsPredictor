package io.github.tjheslin1.dmspredictor.util

import cats.Eq
import cats.syntax.eq._

object ListOps {

  implicit class ListOps[T](val list: List[T]) extends AnyVal {
    def replace(t: T)(implicit eq: Eq[T]): List[T] = list.map {
      case e if e === t => t
      case e            => e
    }

    def replace(ts: List[T])(implicit eq: Eq[T]): List[T] =
      ts.foldLeft(list) {
        case (tss, t) => tss.replace(t)
      }

    def except(t: T): List[T] = list diff List(t)
  }
}
