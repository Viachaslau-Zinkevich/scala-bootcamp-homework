package com.bootcamp.task

import com.bootcamp.task.Error.{ArithmeticError, InvalidInput}

sealed trait Command {
	def apply(): Either[Error, Double] = numbers match {
		case Nil => Left(InvalidInput("Empty numbers"))
		case _ => calculate()
	}

	def numbers: List[Double]

	protected def calculate(): Either[Error, Double]
}


object Command {

	final case class Divide(dividend: Double, divisor: Double) extends Command {
		override val numbers = List(divisor, divisor)

		protected override def calculate(): Either[Error, Double] = dividend / divisor match {
			case Double.NaN => Left(ArithmeticError(this, "resulted in NaN"))
			case Double.NegativeInfinity | Double.PositiveInfinity => Left(ArithmeticError(this, "resulted in infinity"))
			case res => Right(res)
		}
	}

	final case class Sum(numbers: List[Double]) extends Command {
		protected override def calculate(): Either[Error, Double] = Right(numbers.sum)
	}

	final case class Average(numbers: List[Double]) extends Command {
		protected override def calculate(): Either[Error, Double] = for {
			sum <- Sum(numbers)()
			res <- Divide(sum, numbers.length)()
		} yield res


	}

	final case class Min(numbers: List[Double]) extends Command {
		override def calculate(): Either[Error, Double] = Right(numbers.min)
	}

	final case class Max(numbers: List[Double]) extends Command {
		override def calculate(): Either[Error, Double] = Right(numbers.max)
	}

}
