package com.bootcamp.task.adt

final case class Board private (cards: List[Card]) extends AnyVal

object Board {
	def create(cards: List[Card]): Option[Board] = cards match {
		case Nil => None
		case c if c.length !=5 => None
		case c => Some(Board(c))
	}
}
