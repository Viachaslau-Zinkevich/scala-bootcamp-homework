package com.bootcamp.task.adt.test

import com.bootcamp.task.adt.{Board, Card, Hand}

final case class TestCase(board: Board, hands: List[Hand])

object TestCase {
	def create(rawBoard: List[Card], rawHands: List[List[Card]]): Option[TestCase] = {
		for {
			board <- Board.create(rawBoard)
			hands = rawHands.flatMap(Hand.create) if rawHands.length == hands.length
		} yield TestCase(board, hands)
	}
}
