package com.bootcamp.task

import scala.annotation.tailrec

object Homework {

	// try to implement min different ways (fold, reduce, recursion)
	def min(list: List[Int]): Option[Int] = {
		@tailrec
		def recurrentMin(min: Int, tail: List[Int]): Int = tail match {
			case Nil => min
			case head :: tail => recurrentMin(math.min(head, min), tail)
		}
		list match {
			case Nil => None
			case current :: tail => Some(recurrentMin(current, tail))
		}
	}

	// Implement scanLeft (not using scans ofc)
	def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
		list match {
			case Nil => List(zero)
			case l =>
				l.foldLeft(List(zero)) { case (history, item) =>
					val res = f(history.head, item)
					res :: history
				}.reverse
		}
	}

	// https://twitter.com/allenholub/status/1357115515672555520/photo/1
	// pass the interview
	def count(s: String): List[(Char, Int)] = {
		@tailrec
		def countRec(current: Char, count: Int, str: List[Char], res: List[(Char, Int)]): List[(Char, Int)] = {
			str match {
				case Nil => res :+ (current, count)
				case char :: tail =>
					val (nextCount, nextRes) = if(current == char) (count + 1, res) else (1, res :+ (current, count))
					countRec(char, nextCount, tail, nextRes)
			}
		}
		s.toList match {
			case Nil => Nil
			case c :: tail => countRec(c, 1, tail, Nil)
		}
	}


	// hometask:
	// https://leetcode.com/problems/running-sum-of-1d-array/
	def runningSum(nums: Array[Int]): Array[Int] = {
		if (nums == null || nums.length == 0) nums
		else nums.tail.scan(nums.head) (_ + _)
	}

	// https://leetcode.com/problems/shuffle-the-array
	def shuffle(nums: Array[Int], n: Int): Array[Int] = {
		(nums.take(n) zip nums.slice(n, nums.length)).flatMap {case (a, b) => List(a, b)}
	}

	// https://leetcode.com/problems/richest-customer-wealth
	def maximumWealth(accounts: Array[Array[Int]]): Int = {
		val totals = for {
			banks <- accounts
		} yield banks.sum
		totals.max
	}

	// https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
	def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
		val max = candies.max
		candies.map(_ + extraCandies >= max)
	}
	// https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
	def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
		val sorted = points.sortWith((p1, p2) => p1.head <= p2.head)
		(sorted zip sorted.tail).map { case (a,b) => Math.abs(b.head - a.head) }.max
	}
	// optional hometask:
	//
	// https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
	def maxDepth(s: String): Int = {
		@tailrec
		def maxDepthRec(depth: Int, list: List[Char], res: Int): Int = {
			list match {
				case Nil => res max depth
				case el :: tail => el match {
					case '(' => maxDepthRec(depth + 1, tail, res)
					case ')' => maxDepthRec(depth - 1, tail, res max depth)
					case _ => maxDepthRec(depth, tail, res)
				}
			}
		}
		maxDepthRec(0, s.toList, 0)
	}
	// https://leetcode.com/problems/split-a-string-in-balanced-strings
	def balancedStringSplit(s: String): Int = {

		val (res, _) = s.foldLeft((0, (0, 0))) { case ((res, (l,r)), c) =>
			val (nL: Int, nR: Int) = c match {
				case 'L' => (l + 1, r)
				case 'R' => (l, r + 1)
				case _ => ???
			}
			val nRes = if (nL == nR) res + 1 else res
			(nRes, (nL, nR))
		}
		res
	}

	// https://leetcode.com/problems/matrix-block-sum/
	def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
		val m = mat.length
		val n = mat.head.length

		val array = Array.ofDim[Int](mat.length, mat.head.length)

		for {
			i <- 0 until m
			j <- 0 until n
			r <- (0 max (i - K)) to (m - 1 min (i + K))
			c <- (0 max (j - K)) to (n - 1 min (j + K))
		} array(i)(j) += mat(r)(c)
		array
	}
}
