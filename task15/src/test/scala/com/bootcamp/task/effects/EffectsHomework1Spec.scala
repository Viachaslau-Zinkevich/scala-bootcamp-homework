package com.bootcamp.task.effects

import com.bootcamp.task.effects.EffectsHomework1._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger

class EffectsHomework1Spec extends AnyFreeSpec with Matchers {
  "IO map successful" in {
    assert (2 == IO(1).map(_ + 1).unsafeRunSync())
  }

  "IO flatMap successful" in {
    assert (3 == IO(1).flatMap( a => IO(a + 2)).unsafeRunSync())
  }

  "IO *> successfully take right" in {
    val res = new AtomicInteger(0)

    assert (2 == (new IO(() => res.incrementAndGet()) *> IO(res.incrementAndGet())).unsafeRunSync())
  }

  "IO *> right not called" in {
    val res = new AtomicInteger(0)

    assertThrows[RuntimeException]((new IO[AtomicInteger](() => throw new RuntimeException("failure")) *> IO(res.incrementAndGet()))
      .unsafeRunSync())
    assert (0 == res.get())
  }

  "IO as successful" in {
    val res = new AtomicInteger(0)
    assert ("test" == IO(res.incrementAndGet()).as("test").unsafeRunSync())
    assert (1 == res.get())
  }

  "IO void successful" in {
    val res = new AtomicInteger(0)

    IO(res.incrementAndGet()).void.unsafeRunSync()
    assert (1 == res.get())
  }

  "IO attempt" - {
    "happy path" in {

      assert (Right(1) == IO(1).attempt.unsafeRunSync())
    }

    "with error" in {
      val error = new RuntimeException("fake news")

      assert (Left(error) == IO(throw error).attempt.unsafeRunSync())
    }
  }

  "IO option" - {
    "for null value" in {
      assert(IO(null).option.unsafeRunSync().isEmpty)
    }

    "with value" in {
      assert(IO(1).option.unsafeRunSync().contains(1))
    }
  }

  "IO handleErrorWith" - {
    "happy path" in {
      assert (0 == IO(0).handleErrorWith( _ => IO(1)).unsafeRunSync())
    }

    "with error" in {
      assert (1 == IO(throw new RuntimeException()).handleErrorWith( _ => IO(1)).unsafeRunSync())
    }
  }

  "IO redeem" in {

    def create(v: => Int) = IO(v).redeem(_ => -1, _ + 1)

    assert (2 == create(1).unsafeRunSync())
    assert (-1 == create(throw new RuntimeException()).unsafeRunSync())
  }
}
