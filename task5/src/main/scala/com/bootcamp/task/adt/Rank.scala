package com.bootcamp.task.adt

sealed abstract case class Rank(score: Int, code: Char) {
	override def toString: String = code.toString
}

object Rank {
	object A extends Rank(0, 'A')
	object R2 extends Rank(2, '2')
	object R3 extends Rank(3, '3')
	object R4 extends Rank(4, '4')
	object R5 extends Rank(5, '5')
	object R6 extends Rank(6, '6')
	object R7 extends Rank(7, '7')
	object R8 extends Rank(8, '8')
	object R9 extends Rank(9, '9')
	object T extends Rank(10, 'T')
	object J extends Rank(11, 'J')
	object Q extends Rank(12, 'Q')
	object K extends Rank(13, 'K')
}
