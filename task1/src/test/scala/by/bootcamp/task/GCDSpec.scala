package by.bootcamp.task

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class GCDSpec extends AnyFlatSpec {
	"gcd" should "be valid for positive" in {
		assert(GCD(42, 56) == 14)
		assert(GCD(56, 42) == 14)
	}

	it should "be 1 for prime" in {
		assert(GCD(5, 7) == 1)
	}

	it should "be valid for negative" in {
		assert(GCD(-48, 180) == 12)
		assert(GCD(-180, 48) == 12)
		assert(GCD(-180, -48) == 12)
	}

	it should "be valid for zero" in {
		assert(GCD(48, 0) == 48)
		assert(GCD(0, 48) == 48)
	}
}
