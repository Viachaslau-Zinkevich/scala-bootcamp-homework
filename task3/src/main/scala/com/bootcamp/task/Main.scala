package com.bootcamp.task

import com.bootcamp.task.Command._
import com.bootcamp.task.Error.InvalidInput

import scala.io.Source

object Main {

	// This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
	def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println

	def process(input: String): String = {
		val output = for {
			command <- parseCommand(input)
			result <- calculate(command)
		} yield renderResult(result)

		output match {
			case Left(error) => error.value
			case Right(value) => value
		}
	}

	def parseCommand(input: String): Either[Error, Command] = {
		val command :: elements = input.trim.split("\\s+").toList

		val args = elements.map(_.toDoubleOption)
			.takeWhile(_.nonEmpty)
			.flatten

		args match {
			case l if l.length != elements.length => Left(InvalidInput(s"One of $elements is not a valid Double"))
			case Nil => Left(InvalidInput("Empty list of arguments"))
			case l => (command, l) match {
				case ("div", x :: y :: Nil) => Right(Divide(x, y))
				case ("div", elements) => Left(InvalidInput(s"Invalid amount of elements: $elements"))

				case ("sum", elements) => Right(Sum(elements))
				case ("average", elements) => Right(Average(elements))
				case ("min", elements) => Right(Min(elements))
				case ("max", elements) => Right(Max(elements))

				case (other, _) => Left(InvalidInput(s"Invalid command: $other"))
			}
		}
	}

	// should return an error (using `Left` channel) in case of division by zero and other
	// invalid operations
	def calculate(f: Command): Either[Error, Result] =
		f() match {
			case Right(value) => Right(Result(f, value))
			case Left(err) => Left(err)
		}

	def renderResult(res: Result): String = res.command match {
		case Divide(x, y) => s"$x divided by $y is ${res.value}"
		case command =>
			val input = command.numbers.foldLeft("") { (acc, el) => s"$acc $el" }
			s"the ${command.getClass.getName.toLowerCase} of $input is ${res.value}"
	}

	final case class Result(command: Command, value: Double)
}
