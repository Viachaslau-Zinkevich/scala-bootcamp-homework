package com.bootcamp.task.calculator

import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec

import scala.math.BigDecimal.RoundingMode

class SimpleCalculatorSpec extends AnyFreeSpec with EitherValues {
	"calculator" - {
		"enters digits correctly" in {
			assertResult(Right(BigDecimal(1234)))(SimpleCalculator()
				.enter(1)
				.flatMap(_.enter(2))
				.flatMap(_.enter(3))
				.flatMap(_.enter('4'))
				.flatMap(_.result))
		}

		"enters non-digits" in {
			assertResult(Left("Unsupported input:="))(SimpleCalculator()
				.enter('='))

			assertResult(Left("Unsupported input:d"))(SimpleCalculator()
				.enter(1)
				.flatMap(_.enter('d')))

			assertResult(Left("Not a digit"))(SimpleCalculator()
				.enter(12))
		}
	}

	"successful add calculations for" - {
		"single operation" in {
			assertResult(BigDecimal(15))(
				SimpleCalculator().enter(1)
					.flatMap(_.enter(2))
					.flatMap(_.enter('+'))
					.flatMap(_.enter('3'))
					.flatMap(_.result).right.value
			)
		}

		"multiple operations" in {
			assertResult(BigDecimal(15))(
				SimpleCalculator().enter(1)
					.flatMap(_.enter(0))
					.flatMap(_.enter('+'))
					.flatMap(_.enter(3))
					.flatMap(_.enter('+'))
					.flatMap(_.enter(2))
					.flatMap(_.result).right.value
			)
		}
	}

	"successful subtract calculations for" - {
		"single operation" in {
			assertResult(BigDecimal(5))(
				SimpleCalculator().enter(1)
					.flatMap(_.enter(0))
					.flatMap(_.enter('-'))
					.flatMap(_.enter('5'))
					.flatMap(_.result).right.value
			)
		}

		"multiple operations" in {
			assertResult(BigDecimal(-3))(
				SimpleCalculator().enter(1)
					.flatMap(_.enter(0))
					.flatMap(_.enter('-'))
					.flatMap(_.enter(1))
					.flatMap(_.enter(0))
					.flatMap(_.enter('-'))
					.flatMap(_.enter(3))
					.flatMap(_.result).right.value
			)
		}
	}

	"successful multiply calculations for" - {
		"single operation" in {
			assertResult(BigDecimal(6))(
				SimpleCalculator().enter(2)
					.flatMap(_.enter('*'))
					.flatMap(_.enter(3))
					.flatMap(_.result).right.value
			)
		}

		"multiple operations" in {
			assertResult(BigDecimal(24))(
				SimpleCalculator().enter(2)
					.flatMap(_.enter('*'))
					.flatMap(_.enter(3))
					.flatMap(_.enter('*'))
					.flatMap(_.enter(4))
					.flatMap(_.result).right.value
			)
		}
	}

	"division by zero" in {
		assertResult("Division by zero")(
			SimpleCalculator().enter(1)
				.flatMap(_.enter('/'))
				.flatMap(_.enter(0))
				.flatMap(_.result).left.value
		)
	}

	"successful divide calculations for" - {
		"single operation" in {
			assertResult(BigDecimal(0.5))(
				SimpleCalculator().enter(2)
					.flatMap(_.enter('/'))
					.flatMap(_.enter(4))
					.flatMap(_.result).right.value
			)
		}

		"single operation with precision" in {
			assertResult(BigDecimal(0.33))(
				SimpleCalculator().enter(1)
					.flatMap(_.enter('/'))
					.flatMap(_.enter(3))
					.flatMap(_.result).right.value.setScale(2, RoundingMode.FLOOR)
			)
		}

		"multiple operations" in {
			assertResult(BigDecimal(0.25))(
				SimpleCalculator().enter(1)
					.flatMap(_.enter('/'))
					.flatMap(_.enter(2))
					.flatMap(_.enter('/'))
					.flatMap(_.enter(2))
					.flatMap(_.result).right.value
			)
		}
	}

	"successful mixed calculations" in {
		assertResult(BigDecimal(0.4))(
			SimpleCalculator().enter(1)
				.flatMap(_.enter('+'))
				.flatMap(_.enter(2))
				.flatMap(_.enter('*'))
				.flatMap(_.enter(3))
				.flatMap(_.enter('-'))
				.flatMap(_.enter(5))
				.flatMap(_.enter('/'))
				.flatMap(_.enter(1))
				.flatMap(_.enter(0))

				.flatMap(_.result).right.value
		)
	}

	"invalid input for" - {
		"multiple successive operations" in {
			assertResult("Missing second operand")(
				SimpleCalculator().enter(1)
					.flatMap(_.enter('+'))
					.flatMap(_.enter('+'))
					.flatMap(_.enter(1))

					.flatMap(_.result).left.value
			)
		}

		"missing second operand" in {
			assertResult("Missing second operand")(
				SimpleCalculator().enter(1)
					.flatMap(_.enter('+'))

					.flatMap(_.result).left.value
			)
		}
	}
}
