package io.github.tjheslin1.dmspredictor.util

import cats.Eq
import cats.syntax.eq._

object ListOps {

  implicit class ListOps[T: Eq](val list: List[T]) extends AnyVal {
    def replace(t: T): List[T] = list.map {
      case e if e === t => t
      case e => e
    }
  }
}
