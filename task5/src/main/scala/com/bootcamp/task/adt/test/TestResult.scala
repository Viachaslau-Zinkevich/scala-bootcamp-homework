package com.bootcamp.task.adt.test

import com.bootcamp.task.adt.ScoredHand

final case class TestResult private (sortedHands: List[ScoredHand]) {
	override def toString: String = {
		(sortedHands zip sortedHands.tail)
			.foldRight(sortedHands.head.hand.toString) { case ((current, next), res) =>
			res + (if(current.score == next.score)  "=" else " ") + next.hand
		}
	}
}

object TestResult {
	def apply(hands: List[ScoredHand]): TestResult = new TestResult(hands.sortBy(_.score))
}
