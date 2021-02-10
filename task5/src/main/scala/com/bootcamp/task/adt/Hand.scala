package com.bootcamp.task.adt

final case class Hand private (cards: List[Card]) extends AnyVal {
	override def toString: String = cards.mkString("")
}

object Hand {
	def create(cards: List[Card]): Option[Hand] = cards match {
		case Nil => None
		case c if c.length > 5 => None
		case c => Some(Hand(c))
	}
}
