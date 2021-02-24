package com.bootcamp.task

sealed trait Error {
	def value: String
}

object Error {

	final case class ArithmeticError(command: Command, details: String) extends Error {
		override val value: String = s"${command.getClass.getName.toLowerCase} failed for ${command.numbers} with: $details"
	}

	final case class InvalidInput(value: String) extends Error

}
