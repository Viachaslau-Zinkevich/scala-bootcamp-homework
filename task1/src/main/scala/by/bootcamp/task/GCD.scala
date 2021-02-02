package by.bootcamp.task

import scala.annotation.tailrec
import scala.math.abs

object GCD {

    def apply(a: Int, b: Int): Int = calc(abs(a), abs(b))

    @tailrec
    private final def calc(a: Int, b: Int): Int =
      b match {
      case 0 => a
      case _ => calc(b, a % b)
    }
}
