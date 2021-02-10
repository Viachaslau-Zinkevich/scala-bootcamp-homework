package com.bootcamp.task.adt

final case class Card(rank: Rank, suit: Suit) {
	override def toString: String = s"$rank$suit"
}
