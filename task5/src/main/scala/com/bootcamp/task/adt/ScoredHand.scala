package com.bootcamp.task.adt

final case class ScoredHand(hand: Hand, score: Int)

object ScoredHand {
	def apply(hand: Hand): ScoredHand =
		ScoredHand(hand, Rule.score(hand))
}
