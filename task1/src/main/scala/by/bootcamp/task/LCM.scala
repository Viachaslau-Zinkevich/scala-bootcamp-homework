package by.bootcamp.task

import scala.math._
import scala.util.{Failure, Try}

object LCM {
  def apply(a: Int, b: Int): Try[Int] =
    if (a == 0 && b == 0) Failure(new ArithmeticException("No LCM for 0"))
    else Try(calc(abs(a), abs(b)))

  private def calc(a: Int, b: Int): Int =
    multiplyExact(min(a, b), max(a, b) / GCD(a, b))

}
