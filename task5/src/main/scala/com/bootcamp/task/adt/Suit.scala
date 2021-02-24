package com.bootcamp.task.adt

sealed trait Suit

object Suit {

	object Diamonds extends Suit {
		override def toString: String = "D"
	}

	object Hearts extends Suit {
		override def toString: String = "H"
	}

	object Spades extends Suit {
		override def toString: String = "S"
	}

	object Clubs extends Suit {
		override def toString: String = "C"
	}
}
