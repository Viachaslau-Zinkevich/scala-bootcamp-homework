package com.bootcamp.task.error_handling

import cats.data.{Validated, ValidatedNec}

import java.time.YearMonth
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField.{MONTH_OF_YEAR, YEAR}
import scala.util.{Failure, Success, Try}
import cats.syntax.all._

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {

	final case class ClientName(firstName: String, secondName: String)

	final case class CardNumber(number: String) extends AnyVal

	case class PaymentCard(name: ClientName, number: CardNumber, expirationDate: YearMonth)

	sealed trait ValidationError

	object ValidationError {

		case object CardExpired extends ValidationError

		case object InvalidNameFormat extends ValidationError

		case object InvalidNameLength extends ValidationError

		case object NonLetterSymbols extends ValidationError

		case class InvalidDateFormat(details: String) extends ValidationError

		case object NonDigitCardNumberSymbols extends ValidationError

		case object InvalidNumberLength extends ValidationError

		case object InvalidCVVLength extends ValidationError

		case class MissingRequiredField(field: String) extends ValidationError

	}

	object PaymentCardValidator {

		import ValidationError._

		type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

		def validate(
						name: String,
						number: String,
						expirationDate: String,
						securityCode: String,
					): AllErrorsOr[PaymentCard] = (validateClientName(name), validateNumber(number),
			validateExpirationDate(expirationDate, YearMonth.now))
			.mapN(PaymentCard)
			.andThen(card => validateCVV(card, securityCode))

		private def validateCVV(card: PaymentCard, cvv: String): AllErrorsOr[PaymentCard] = {
			val number = card.number.number

			val validLength = number.head match {
				case 3 if number.length == 15 && (Set('4', '7') contains number.charAt(1)) &&
					cvv.length == 4 => Validated.validNec()
				case _ if cvv.length == 3 => Validated.validNec()
				case _ => InvalidCVVLength.invalidNec
			}

			// Bank validation of card data with cvv goes here
			def validateCardWithCVV = card.validNec

			validLength.andThen(_ => validateCardWithCVV)
		}

		private def validateExpirationDate(expirationDate: String, currentDate: YearMonth): AllErrorsOr[YearMonth] = {

			def validateFormat: AllErrorsOr[YearMonth] = {
				val formatter = new DateTimeFormatterBuilder()
					.appendValue(MONTH_OF_YEAR, 2)
					.appendLiteral('/')
					.appendValue(YEAR, 2)
					.toFormatter

				Try(YearMonth.parse(expirationDate, formatter)) match {
					case Success(date) => date.validNec
					case Failure(exception) => InvalidDateFormat(exception.getMessage).invalidNec
				}
			}

			def validateDate(date: YearMonth): AllErrorsOr[YearMonth] = {
				if (currentDate isBefore date) date.validNec
				else CardExpired.invalidNec
			}

			validateFormat andThen validateDate
		}

		private def validateNumber(number: String): AllErrorsOr[CardNumber] = {
			val normalizedNumber = number.trim().replaceAll("[-/\\\\]", "")
			val onlyDigits: AllErrorsOr[String] = if (normalizedNumber.chars().allMatch(Character.isDigit(_))) normalizedNumber.validNec
			else NonDigitCardNumberSymbols.invalidNec

			def validateLength: AllErrorsOr[String] = normalizedNumber.head.toInt match {
				case 4 if normalizedNumber.length == 13 || normalizedNumber.length == 16 => normalizedNumber.validNec
				case 5 if normalizedNumber.length == 16 => normalizedNumber.validNec
				case 3 if normalizedNumber.length == 15 && (Set('4', '7') contains normalizedNumber.charAt(1)) => normalizedNumber.validNec
				case 3 if normalizedNumber.length == 14 && (Set('0', '6', '8') contains normalizedNumber.charAt(1)) => normalizedNumber.validNec
				case 6 if normalizedNumber.length == 16 => normalizedNumber.validNec
				case _ => InvalidNumberLength.invalidNec
			}

			// Luhn algo goes here
			def verifyCheckDigit: AllErrorsOr[String] = normalizedNumber.validNec

			onlyDigits.andThen(_ => validateLength *> verifyCheckDigit)
				.map(CardNumber)
		}

		private def validateClientName(name: String): AllErrorsOr[ClientName] = {
			val nameSize: AllErrorsOr[String] = if (2 until 26 contains name.length) name.validNec
			else InvalidNameLength.invalidNec

			val letterSymbols: AllErrorsOr[String] = if (!name.chars().allMatch(Character.isLetter(_))) NonLetterSymbols.invalidNec
			else name.validNec

			val disjoinedName = name.split("\\s+")
			val fullName: AllErrorsOr[ClientName] = disjoinedName match {
				case Array(first, second) => ClientName(first, second).validNec
				case _ => InvalidNameFormat.invalidNec
			}

			nameSize *> letterSymbols *> fullName
		}
	}

}
