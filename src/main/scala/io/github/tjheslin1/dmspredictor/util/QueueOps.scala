package io.github.tjheslin1.dmspredictor.util

import scala.collection.immutable.Queue

object QueueOps {

  implicit class QueueOps[T](val queue: Queue[T]) extends AnyVal {
    def append(t: T): Queue[T] = queue.:+(t)
  }
}
