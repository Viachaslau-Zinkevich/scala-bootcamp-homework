package com.bootcamp.task.adt

sealed trait Rule {
	def apply(hand: Hand): Option[Int] = if(isApplicable(hand)) Some(score(hand)) else None

	protected def score(hand: Hand): Int

	protected def isApplicable(hand: Hand): Boolean
}

object Rule {

	sealed abstract case class BaseRule(rank: Int) extends Rule {
		// Impl should be more complex to include cards
		override def score(hand: Hand): Int = rank
	}

	// dumb impl just to be
	def score(hand: Hand): Int =
		StraightFlush(hand)
			.orElse(Flush(hand))
			.orElse(Quadruple(hand))
			.orElse(FullHouse(hand))
			.orElse(Straight(hand))
			.orElse(DoublePair(hand))
			.orElse(Triple(hand))
			.orElse(Pair(hand))
			.getOrElse(1)

	final object Pair extends BaseRule(2) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object Triple extends BaseRule(4) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object DoublePair extends BaseRule(3) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object Quadruple extends BaseRule(9) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object FullHouse extends BaseRule(8) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object Straight extends BaseRule(6) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object Flush extends BaseRule(7) {
		override def isApplicable(hand: Hand): Boolean = ???
	}

	final object StraightFlush extends BaseRule(10) {
		override def isApplicable(hand: Hand): Boolean = ???
	}
}
