package by.bootcamp.task

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.{Failure, Success}

class LCMSpec extends AnyFlatSpec {
	"lcm" should "be valid for positive" in {
		assert(LCM(4, 6) == Success(12))
		assert(LCM(6, 4) == Success(12))
	}

	it should "be valid for same value" in {
		assert(LCM(4, 4) == Success(4))
		assert(LCM(-4, -4) == Success(4))
	}

	it should "be valid for negative" in {
		assert(LCM(-21, 6) == Success(42))
		assert(LCM(6, -21) == Success(42))
		assert(LCM(-6, -21) == Success(42))
	}

	it should "fail with ArithmeticException for huge numbers" in {
		assert(LCM(2147483647, 2147483644).isFailure)
	}

	it should "succeed for single 0" in {
		assert(LCM(0, 1) == Success(0))
		assert(LCM(1, 0) == Success(0))
	}

	it should "fail with ArithmeticException for both 0" in {
		assert(LCM(0, 0).isFailure)
	}
}
