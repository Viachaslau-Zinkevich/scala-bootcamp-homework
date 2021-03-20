package com.bootcamp.task.calculator

import com.bootcamp.task.calculator.SimpleCalculator._

import scala.math.BigDecimal

case class SimpleCalculator private (memory: Value) extends AnyVal {

	def enter(digit: Int): Either[Error, SimpleCalculator] = digit match {
		case d if d >=0 && d <=9 => enter(Digit(d.toByte))
		case _ => Left(s"Not a digit")
	}

	def enter(char: Char): Either[Error, SimpleCalculator] = char match {
		case c if c.isDigit => enter(Digit(c.asDigit.toByte))
		case '+' => enter(Add)
		case '-' => enter(Subtract)
		case '*' => enter(Multiply)
		case '/' => enter(Divide)
		case c => Left(s"Unsupported input:$c")
	}

	private def enter(entry: Entry): Either[Error, SimpleCalculator] = memory.apply(entry).map(SimpleCalculator(_))

	def result: Either[Error, BigDecimal] = memory.calculate
}

object SimpleCalculator {

	def apply(): SimpleCalculator = SimpleCalculator(Result(0))

	type Error = String
	sealed trait Entry

	sealed trait Value extends Entry {
		def apply(entry: Entry): Either[Error, Value]

		def calculate: Either[Error, BigDecimal]
	}

	case class Digit(value: Byte) extends Value {
		override def apply(entry: Entry): Either[Error, Value] = entry match {
			case Digit(d) => Right(Result(value * 10 + d))
			case o: Operation => Right(Intermediate(Result(value), o, None))
			case e => Left(s"invalid entry:${e}")
		}

		override def calculate: Either[Error, BigDecimal] = Right(BigDecimal(value))
	}
	case class Intermediate(op1: Result, operation: Operation, op2: Option[Result]) extends Value {
		override def apply(entry: Entry): Either[Error, Value] = entry match {
			case d: Digit => Right(this.copy(op2 = op2.orElse(Some(Result(0))).map(_.apply(d))))
			case o: Operation => this.toResult.map(Intermediate(_, o, None))
			case e => Left(s"Only digits and operations are supported:${e}")
		}

		def toResult: Either[Error, Result] = op2.toRight("Missing second operand")
			.flatMap(operation.apply(op1, _))

		override def calculate: Either[Error, BigDecimal] = toResult.map(_.value)
	}

	case class Result(value: BigDecimal) extends Value {
		override def apply(entry: Entry): Either[Error, Value] = entry match {
			case d: Digit => Right(apply(d))
			case o: Operation => Right(Intermediate(this, o, None))
			case e => Left(s"Only digits and operations are supported:${e}")
		}

		def apply(digit: Digit): Result = Result(value * 10 + digit.value)

		override def calculate: Either[Error, BigDecimal] = Right(value)
	}

	sealed trait Operation extends Entry {
		def apply(op1: Result, op2: Result): Either[Error, Result]
	}

	object Add extends Operation {
		override def apply(op1: Result, op2: Result): Either[Error, Result] = Right(Result(op1.value + op2.value))
	}
	object Subtract extends Operation {
		override def apply(op1: Result, op2: Result): Either[Error, Result] = Right(Result(op1.value - op2.value))
	}
	object Multiply extends Operation {
		override def apply(op1: Result, op2: Result): Either[Error, Result] = Right(Result(op1.value * op2.value))
	}
	object Divide extends Operation {
		override def apply(op1: Result, op2: Result): Either[Error, Result] = op2 match {
			case Result(r) if r.isWhole && r == 0 => Left("Division by zero")
			case Result(r) => Right(Result(op1.value / r))
		}
	}
}
